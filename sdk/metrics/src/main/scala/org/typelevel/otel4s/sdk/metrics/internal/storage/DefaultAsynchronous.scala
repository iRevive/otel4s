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

package org.typelevel.otel4s.sdk.metrics.internal.storage

import cats.Monad
import cats.effect.Concurrent
import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.std.Console
import cats.mtl.Ask
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.context.AskContext
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.metrics.Aggregation
import org.typelevel.otel4s.sdk.metrics.data.AggregationTemporality
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.data.TimeWindow
import org.typelevel.otel4s.sdk.metrics.internal.InstrumentDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.Measurement
import org.typelevel.otel4s.sdk.metrics.internal.MetricDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.aggregation.Aggregator
import org.typelevel.otel4s.sdk.metrics.internal.exporter.RegisteredReader
import org.typelevel.otel4s.sdk.metrics.internal.storage.MetricStorage.Asynchronous
import org.typelevel.otel4s.sdk.metrics.view.AttributesProcessor
import org.typelevel.otel4s.sdk.metrics.view.RegisteredView

import scala.concurrent.duration.FiniteDuration

private final class DefaultAsynchronous[
    F[_]: Monad: Console: AskContext,
    A
] private (
    val reader: RegisteredReader[F],
    val metricDescriptor: MetricDescriptor,
    aggregationTemporality: AggregationTemporality,
    aggregator: Aggregator.Asynchronous[F, A],
    attributesProcessor: AttributesProcessor,
    maxCardinality: Int,
    collector: DefaultAsynchronous.Collector[F, A]
) extends Asynchronous[F, A] {

  def record(m: Measurement[A]): F[Unit] =
    for {
      context <- Ask[F, Context].ask
      attributes = attributesProcessor.process(m.attributes, context)
      start <- collector.startTimestamp(m)
      measurement = m.withAttributes(attributes).withStartTimestamp(start)
      points <- collector.currentPoints
      _ <- {
        if (points.contains(attributes)) {
          Console[F].errorln(
            s"Instrument ${metricDescriptor.sourceInstrument.name} has recorded multiple values for the same attributes $attributes"
          )
        } else {
          if (points.sizeIs >= maxCardinality) {
            cardinalityWarning >> collector.record(
              measurement.withAttributes(
                attributes.added(MetricStorage.OverflowAttribute)
              )
            )
          } else {
            collector.record(measurement)
          }
        }
      }
    } yield ()

  def collect(
      resource: TelemetryResource,
      scope: InstrumentationScope,
      timeWindow: TimeWindow
  ): F[Option[MetricData]] =
    collector.collectPoints.flatMap { measurements =>
      aggregator
        .toMetricData(
          measurements,
          resource,
          scope,
          metricDescriptor,
          aggregationTemporality
        )
        .map(Some(_))
    }

  private def cardinalityWarning: F[Unit] =
    Console[F].errorln(
      s"Instrument [${metricDescriptor.sourceInstrument.name}] has exceeded the maximum allowed cardinality [$maxCardinality]"
    )
}

private object DefaultAsynchronous {

  def create[
      F[_]: Temporal: Console: AskContext,
      A: MeasurementValue: Numeric
  ](
      reader: RegisteredReader[F],
      registeredView: RegisteredView,
      instrumentDescriptor: InstrumentDescriptor.Asynchronous,
      aggregation: Aggregation.Asynchronous
  ): F[Asynchronous[F, A]] = {
    val view = registeredView.view
    val descriptor = MetricDescriptor(view, instrumentDescriptor)

    val aggregator: Aggregator.Asynchronous[F, A] =
      Aggregator.asynchronous(aggregation, instrumentDescriptor)

    val aggregationTemporality =
      reader.reader.aggregationTemporalitySelector.select(
        descriptor.sourceInstrument.instrumentType
      )

    for {
      collector <- Collector.create[F, A](
        aggregationTemporality,
        reader,
        aggregator
      )
    } yield new DefaultAsynchronous[F, A](
      reader,
      descriptor,
      aggregationTemporality,
      aggregator,
      registeredView.viewAttributesProcessor,
      registeredView.cardinalityLimit - 1,
      collector
    )
  }

  private trait Collector[F[_], A] {
    def startTimestamp(measurement: Measurement[A]): F[FiniteDuration]
    def record(measurement: Measurement[A]): F[Unit]
    def currentPoints: F[Map[Attributes, Measurement[A]]]
    def collectPoints: F[Vector[Measurement[A]]]
  }

  private object Collector {

    def create[F[_]: Concurrent, A](
        aggregationTemporality: AggregationTemporality,
        reader: RegisteredReader[F],
        aggregator: Aggregator.Asynchronous[F, A]
    ): F[Collector[F, A]] =
      aggregationTemporality match {
        case AggregationTemporality.Delta => delta[F, A](reader, aggregator)
        case AggregationTemporality.Cumulative => cumulative[F, A]
      }

    private def delta[F[_]: Concurrent, A](
        reader: RegisteredReader[F],
        aggregator: Aggregator.Asynchronous[F, A]
    ): F[Collector[F, A]] =
      Ref.of(Map.empty[Attributes, Measurement[A]]).flatMap { pointsRef =>
        Ref.of(Map.empty[Attributes, Measurement[A]]).map { lastPointsRef =>
          new Collector[F, A] {
            def startTimestamp(measurement: Measurement[A]): F[FiniteDuration] =
              reader.getLastCollectTimestamp

            def record(measurement: Measurement[A]): F[Unit] =
              pointsRef.update(_.updated(measurement.attributes, measurement))

            def currentPoints: F[Map[Attributes, Measurement[A]]] =
              pointsRef.get

            def collectPoints: F[Vector[Measurement[A]]] =
              for {
                points <- pointsRef.get
                lastPoints <- lastPointsRef.getAndSet(points)
              } yield points.toVector.map { case (k, v) =>
                lastPoints.get(k) match {
                  case Some(lastPoint) =>
                    aggregator.diff(lastPoint, v)
                  case None =>
                    v
                }
              }
          }
        }
      }

    private def cumulative[F[_]: Concurrent, A]: F[Collector[F, A]] =
      Ref.of(Map.empty[Attributes, Measurement[A]]).map { pointsRef =>
        new Collector[F, A] {
          def startTimestamp(measurement: Measurement[A]): F[FiniteDuration] =
            Monad[F].pure(measurement.timeWindow.start)

          def record(measurement: Measurement[A]): F[Unit] =
            pointsRef.update(_.updated(measurement.attributes, measurement))

          def currentPoints: F[Map[Attributes, Measurement[A]]] =
            pointsRef.get

          def collectPoints: F[Vector[Measurement[A]]] =
            pointsRef.getAndSet(Map.empty).map(_.values.toVector)
        }
      }

  }

}
