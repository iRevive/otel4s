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
import org.typelevel.otel4s.sdk.metrics.data.{
  AggregationTemporality,
  Data,
  ExemplarData,
  MetricData,
  PointData
}
import org.typelevel.otel4s.sdk.metrics.internal.MetricDescriptor

import scala.concurrent.duration.FiniteDuration

private final class LastValueAggregator[F[_]: Concurrent, A](
    val builder: PointDataBuilder[A]
) extends Aggregator[F, A] {

  import LastValueAggregator.Handle

  type Point = builder.Point

  def createHandle: F[Aggregator.Handle[F, A, Point]] =
    for {
      current <- Current.make[F, A]
    } yield new Handle[F, A, Point, builder.Exemplar](current, builder)

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
    new LastValueAggregator(PointDataBuilder[A])

  private class Handle[
      F[_]: Monad,
      A,
      P <: PointData.NumberPoint,
      E <: ExemplarData
  ](
      current: Current[F, A],
      builder: PointDataBuilder.Aux[A, P, E]
  ) extends Aggregator.Handle[F, A, P] {

    def aggregate(
        startTimestamp: FiniteDuration,
        collectTimestamp: FiniteDuration,
        attributes: Attributes,
        reset: Boolean
    ): F[Option[P]] =
      current.get(reset).map { value =>
        value.map { v =>
          builder.create(
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
