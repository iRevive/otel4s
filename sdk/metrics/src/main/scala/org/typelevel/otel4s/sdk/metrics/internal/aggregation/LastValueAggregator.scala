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

package org.typelevel.otel4s.sdk.metrics.internal.aggregation

import cats.Monad
import cats.effect.Concurrent
import cats.syntax.functor._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.metrics.data.AggregationTemporality
import org.typelevel.otel4s.sdk.metrics.data.Data
import org.typelevel.otel4s.sdk.metrics.data.ExemplarData
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.data.PointData
import org.typelevel.otel4s.sdk.metrics.internal.{Measurement, MetricDescriptor}
import org.typelevel.otel4s.sdk.metrics.internal.utils.Current

import scala.concurrent.duration.FiniteDuration

private final class LastValueAggregator[
    F[_]: Concurrent,
    A,
    P <: PointData.NumberPoint,
    E <: ExemplarData
](
    make: PointData.NumberPoint.Make[A, P, E],
) extends Aggregator[F, A] {

  import LastValueAggregator.Accumulator

  type Point = P

  def createAccumulator: F[Aggregator.Accumulator[F, A, Point]] =
    for {
      current <- Current.create[F, A]
    } yield new Accumulator[F, A, Point, E](current, make)

  def toPointData(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      value: A
  ): Option[P] =
    Some(
      make.make(
        startTimestamp,
        collectTimestamp,
        attributes,
        Vector.empty,
        value
      )
    )

  def diff(previous: Measurement[A], current: Measurement[A]): Measurement[A] =
    current

  def toMetricData(
      resource: TelemetryResource,
      scope: InstrumentationScope,
      descriptor: MetricDescriptor,
      points: Vector[Point],
      temporality: AggregationTemporality
  ): F[MetricData] =
    Monad[F].pure(
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

private object LastValueAggregator {

  def apply[F[_]: Concurrent, A: MeasurementValue]: Aggregator[F, A] =
    MeasurementValue[A] match {
      case MeasurementValue.LongMeasurementValue(_) =>
        new LastValueAggregator(PointData.NumberPoint.Make.makeLong)
      case MeasurementValue.DoubleMeasurementValue(_) =>
        new LastValueAggregator(PointData.NumberPoint.Make.makeDouble)
    }

  private class Accumulator[
      F[_]: Monad,
      A,
      P <: PointData.NumberPoint,
      E <: ExemplarData
  ](
      current: Current[F, A],
      make: PointData.NumberPoint.Make[A, P, E]
  ) extends Aggregator.Accumulator[F, A, P] {

    def aggregate(
        startTimestamp: FiniteDuration,
        collectTimestamp: FiniteDuration,
        attributes: Attributes,
        reset: Boolean
    ): F[Option[P]] =
      current.get(reset).map { value =>
        value.map { v =>
          make.make(
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
