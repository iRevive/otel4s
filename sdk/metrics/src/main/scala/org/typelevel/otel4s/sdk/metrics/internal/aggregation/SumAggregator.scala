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
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.metrics.ExemplarFilter
import org.typelevel.otel4s.sdk.metrics.data.AggregationTemporality
import org.typelevel.otel4s.sdk.metrics.data.Data
import org.typelevel.otel4s.sdk.metrics.data.ExemplarData
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.data.PointData
import org.typelevel.otel4s.sdk.metrics.internal.ExemplarReservoir
import org.typelevel.otel4s.sdk.metrics.internal.MetricDescriptor

import scala.concurrent.duration.FiniteDuration

private final class SumAggregator[
    F[_]: Concurrent,
    A: MeasurementValue: Numeric
](
    reservoirSize: Int,
    filter: ExemplarFilter,
    val builder: PointDataBuilder[A]
) extends Aggregator[F, A] {

  import SumAggregator.Handle

  type Point = builder.Point

  def createHandle: F[Aggregator.Handle[F, A, Point]] =
    for {
      adder <- Adder.make[F, A]
      reservoir <- makeReservoir
    } yield new Handle[F, A, Point, builder.Exemplar](
      adder,
      reservoir,
      builder
    )

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
        Data.Sum(points, true, temporality) // todo isMonotonic?
      )
    )

  // todo size = availableProcessors ???
  private def makeReservoir =
    ExemplarReservoir
      .fixedSize[F, A, builder.Exemplar](size = reservoirSize)
      .map(r => ExemplarReservoir.filtered(filter, r))

}

private object SumAggregator {

  def apply[F[_]: Concurrent, A: MeasurementValue: Numeric](
      reservoirSize: Int,
      filter: ExemplarFilter
  ): Aggregator[F, A] =
    new SumAggregator(reservoirSize, filter, PointDataBuilder[A])

  private class Handle[
      F[_]: Monad,
      A,
      P <: PointData.NumberPoint,
      E <: ExemplarData
  ](
      adder: Adder[F, A],
      reservoir: ExemplarReservoir[F, A, E],
      target: PointDataBuilder.Aux[A, P, E]
  ) extends Aggregator.Handle[F, A, P] {

    def aggregate(
        startTimestamp: FiniteDuration,
        collectTimestamp: FiniteDuration,
        attributes: Attributes,
        reset: Boolean
    ): F[Option[P]] =
      for {
        value <- adder.sum(reset)
        exemplars <- reservoir.collectAndReset(attributes)
      } yield Some(
        target.create(
          startTimestamp,
          collectTimestamp,
          attributes,
          exemplars,
          value
        )
      )

    def record(
        value: A,
        attributes: Attributes,
        context: Context
    ): F[Unit] =
      reservoir.offerMeasurement(value, attributes, context) >> adder.add(value)

  }

}
