/*
 * Copyright 2024 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.typelevel.otel4s.sdk.metrics

import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.std.Console
import cats.effect.std.Mutex
import cats.effect.std.Random
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.context.AskContext
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.internal.CallbackRegistration
import org.typelevel.otel4s.sdk.metrics.internal.InstrumentDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.MetricStorageRegistry
import org.typelevel.otel4s.sdk.metrics.internal.SdkObservableMeasurement
import org.typelevel.otel4s.sdk.metrics.internal.exemplar.TraceContextLookup
import org.typelevel.otel4s.sdk.metrics.internal.exporter.RegisteredReader
import org.typelevel.otel4s.sdk.metrics.internal.storage.MetricStorage

import scala.concurrent.duration.FiniteDuration

private[metrics] final class MeterSharedState[
    F[_]: Temporal: Random: Console: AskContext
](
    mutex: Mutex[F],
    resource: TelemetryResource,
    val scope: InstrumentationScope,
    startTimestamp: FiniteDuration,
    exemplarFilter: ExemplarFilter,
    traceContextLookup: TraceContextLookup,
    callbacks: Ref[F, Vector[CallbackRegistration[F]]],
    registries: Map[RegisteredReader[F], MetricStorageRegistry[F]]
) {

  def registerMetricStorage[A: MeasurementValue: Numeric](
      descriptor: InstrumentDescriptor.Synchronous
  ): F[MetricStorage.Writeable[F, A]] =
    registries.toVector
      .flatTraverse { case (reader, registry) =>
        reader.viewRegistry
          .findViews(descriptor, scope)
          .flatTraverse { registeredView =>
            registeredView.view.aggregation match {
              case aggregation: Aggregation.Synchronous =>
                for {
                  storage <- MetricStorage.synchronous(
                    reader,
                    registeredView,
                    descriptor,
                    exemplarFilter,
                    traceContextLookup,
                    aggregation
                  )
                  _ <- registry.register(storage)
                } yield Vector(storage)

              case _ =>
                Temporal[F].pure(Vector.empty[MetricStorage.Synchronous[F, A]])
            }
          }
      }
      .map { storages =>
        MetricStorage.Writeable.of(storages: _*)
      }

  def registerObservableMeasurement[A: MeasurementValue: Numeric](
      descriptor: InstrumentDescriptor.Observable
  ): F[SdkObservableMeasurement[F, A]] =
    registries.toVector
      .flatTraverse { case (reader, registry) =>
        reader.viewRegistry
          .findViews(descriptor, scope)
          .flatTraverse { registeredView =>
            registeredView.view.aggregation match {
              case aggregation: Aggregation.Observable =>
                for {
                  storage <- MetricStorage.observable(
                    reader,
                    registeredView,
                    descriptor,
                    aggregation
                  )
                  _ <- registry.register(storage)
                } yield Vector(storage)

              case _ =>
                Temporal[F].pure(Vector.empty[MetricStorage.Observable[F, A]])
            }
          }
      }
      .flatMap { storages =>
        SdkObservableMeasurement.create(storages, scope, descriptor)
      }

  def collectAll(
      reader: RegisteredReader[F],
      collectTimestamp: FiniteDuration
  ): F[Vector[MetricData]] =
    callbacks.get.flatMap { currentCallbacks =>
      mutex.lock.surround {
        currentCallbacks
          .traverse_ { callback =>
            callback.invokeCallback(reader, startTimestamp, collectTimestamp)
          }
          .flatMap { _ =>
            for {
              storages <- registries.get(reader).foldMapA(_.allStorages)
              result <- storages.traverse { storage =>
                storage.collect(
                  resource,
                  scope,
                  startTimestamp,
                  collectTimestamp
                )
              }
            } yield result.flatten.filter(_.nonEmpty)
          }

      }
    }

  def removeCallback(callback: CallbackRegistration[F]): F[Unit] =
    callbacks.update(_.filter(_ != callback))

  def registerCallback(callback: CallbackRegistration[F]): F[Unit] =
    callbacks.update(_ :+ callback)

}

private object MeterSharedState {

  def create[F[_]: Temporal: Random: Console: AskContext](
      resource: TelemetryResource,
      scope: InstrumentationScope,
      startTimestamp: FiniteDuration,
      exemplarFilter: ExemplarFilter,
      traceContextLookup: TraceContextLookup,
      registeredReaders: Vector[RegisteredReader[F]]
  ): F[MeterSharedState[F]] =
    for {
      mutex <- Mutex[F]
      callbacks <- Ref.empty[F, Vector[CallbackRegistration[F]]]
      registries <- registeredReaders.traverse { reader =>
        MetricStorageRegistry.create[F].tupleLeft(reader)
      }
    } yield new MeterSharedState(
      mutex,
      resource,
      scope,
      startTimestamp,
      exemplarFilter,
      traceContextLookup,
      callbacks,
      registries.toMap
    )
}
