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

package org.typelevel.otel4s.sdk.metrics.internal
package aggregation

import cats.Applicative
import cats.effect.Concurrent
import cats.syntax.functor._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.metrics.data.AggregationTemporality
import org.typelevel.otel4s.sdk.metrics.data.Data
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.internal.utils.Current

import scala.concurrent.duration.FiniteDuration

object LastValueAggregator {

  def synchronous[
      F[_]: Concurrent,
      A: MeasurementValue
  ]: Aggregator.Synchronous[F, A] =
    new Synchronous[F, A]

  def observable[
      F[_]: Applicative,
      A: MeasurementValue
  ]: Aggregator.Observable[F, A] =
    new Observable[F, A]

  private final class Synchronous[
      F[_]: Concurrent,
      A: MeasurementValue
  ] extends Aggregator.Synchronous[F, A] {
    val target: Target[A] = Target[A]

    type Point = target.Point

    def createAccumulator: F[Aggregator.Accumulator[F, A, Point]] =
      for {
        current <- Current.create[F, A]
      } yield new Accumulator(current)

    def toMetricData(
        resource: TelemetryResource,
        scope: InstrumentationScope,
        descriptor: MetricDescriptor,
        points: Vector[Point],
        temporality: AggregationTemporality
    ): F[MetricData] =
      Concurrent[F].pure(
        MetricData(
          resource,
          scope,
          descriptor.name,
          descriptor.description,
          descriptor.sourceInstrument.unit,
          Data.Gauge(points)
        )
      )

    private class Accumulator(
        current: Current[F, A]
    ) extends Aggregator.Accumulator[F, A, Point] {

      def aggregate(
          startTimestamp: FiniteDuration,
          collectTimestamp: FiniteDuration,
          attributes: Attributes,
          reset: Boolean
      ): F[Option[Point]] =
        current.get(reset).map { value =>
          value.map { v =>
            target.makePointData(
              startTimestamp,
              collectTimestamp,
              attributes,
              Vector.empty,
              v
            )
          }
        }

      def record(value: A, attributes: Attributes, context: Context): F[Unit] =
        current.set(value)
    }
  }

  private final class Observable[
      F[_]: Applicative,
      A: MeasurementValue
  ] extends Aggregator.Observable[F, A] {

    private val target: Target[A] = Target[A]

    def diff(
        previous: Measurement[A],
        current: Measurement[A]
    ): Measurement[A] =
      current

    def toMetricData(
        measurements: Vector[Measurement[A]],
        resource: TelemetryResource,
        scope: InstrumentationScope,
        descriptor: MetricDescriptor,
        temporality: AggregationTemporality
    ): F[MetricData] = {
      val points = measurements.map { m =>
        target.makePointData(
          m.startTimestamp,
          m.collectTimestamp,
          m.attributes,
          Vector.empty,
          m.value
        )
      }

      Applicative[F].pure(
        MetricData(
          resource,
          scope,
          descriptor.name,
          descriptor.description,
          descriptor.sourceInstrument.unit,
          Data.Gauge(points)
        )
      )
    }

  }

}
