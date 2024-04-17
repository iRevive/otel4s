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

import cats.Applicative
import cats.effect.Temporal
import cats.effect.std.Console
import cats.effect.std.Random
import cats.syntax.foldable._
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.context.AskContext
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.metrics.Aggregation
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.data.TimeWindow
import org.typelevel.otel4s.sdk.metrics.exemplar.ExemplarFilter
import org.typelevel.otel4s.sdk.metrics.exemplar.TraceContextLookup
import org.typelevel.otel4s.sdk.metrics.internal.AsynchronousMeasurement
import org.typelevel.otel4s.sdk.metrics.internal.InstrumentDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.MetricDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.exporter.RegisteredReader
import org.typelevel.otel4s.sdk.metrics.view.View

private[metrics] trait MetricStorage[F[_]] {
  def metricDescriptor: MetricDescriptor
  def collect(
      resource: TelemetryResource,
      scope: InstrumentationScope,
      timeWindow: TimeWindow
  ): F[Option[MetricData]]
}

private[metrics] object MetricStorage {

  private[storage] val OverflowAttribute: Attribute[Boolean] =
    Attribute("otel.metric.overflow", true)

  trait Writeable[F[_], A] {
    def record(value: A, attributes: Attributes, context: Context): F[Unit]
  }

  object Writeable {
    def of[F[_]: Applicative, A](
        storages: Vector[Writeable[F, A]]
    ): Writeable[F, A] =
      new Writeable[F, A] {
        def record(
            value: A,
            attributes: Attributes,
            context: Context
        ): F[Unit] =
          storages.traverse_(_.record(value, attributes, context))
      }
  }

  trait Synchronous[F[_], A] extends MetricStorage[F] with Writeable[F, A]

  trait Asynchronous[F[_], A] extends MetricStorage[F] {
    def record(measurement: AsynchronousMeasurement[A]): F[Unit]
    def reader: RegisteredReader[F]
  }

  def synchronous[
      F[_]: Temporal: Console: Random,
      A: MeasurementValue: Numeric
  ](
      reader: RegisteredReader[F],
      view: Option[View],
      instrumentDescriptor: InstrumentDescriptor.Synchronous,
      exemplarFilter: ExemplarFilter,
      traceContextLookup: TraceContextLookup,
      aggregation: Aggregation.Synchronous
  ): F[Synchronous[F, A]] =
    DefaultSynchronous.create(
      reader,
      view,
      instrumentDescriptor,
      exemplarFilter,
      traceContextLookup,
      aggregation
    )

  def asynchronous[
      F[_]: Temporal: Console: AskContext,
      A: MeasurementValue: Numeric
  ](
      reader: RegisteredReader[F],
      view: Option[View],
      instrumentDescriptor: InstrumentDescriptor.Asynchronous,
      aggregation: Aggregation.Asynchronous
  ): F[Asynchronous[F, A]] =
    DefaultAsynchronous.create(
      reader,
      view,
      instrumentDescriptor,
      aggregation
    )

}
