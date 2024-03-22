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
package internal.aggregation

import cats.Monad
import cats.effect.Temporal
import cats.effect.std.Random
import cats.syntax.flatMap._
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
import org.typelevel.otel4s.sdk.metrics.internal.MetricDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.exemplar.ExemplarReservoir
import org.typelevel.otel4s.sdk.metrics.internal.exemplar.TraceContextLookup
import org.typelevel.otel4s.sdk.metrics.internal.utils.Adder

import scala.concurrent.duration.FiniteDuration

private final class SumSynchronous[
    F[_]: Temporal: Random,
    A: MeasurementValue: Numeric,
    P <: PointData.NumberPoint,
    E <: ExemplarData
](
    reservoirSize: Int,
    filter: ExemplarFilter,
    traceContextLookup: TraceContextLookup,
    makeNumberPoint: PointData.NumberPoint.Make[A, P, E],
    makeExemplar: ExemplarData.Make[A, E]
) extends Aggregator.Synchronous[F, A] {
  private implicit val make: ExemplarData.Make[A, E] = makeExemplar

  import SumSynchronous.Accumulator

  type Point = P

  def createAccumulator: F[Aggregator.Accumulator[F, A, Point]] =
    for {
      adder <- Adder.create[F, A]
      reservoir <- makeReservoir
    } yield new Accumulator[F, A, Point, E](
      adder,
      reservoir,
      makeNumberPoint
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
        Data.Sum(points, isMonotonic(descriptor), temporality)
      )
    )

  private def isMonotonic(descriptor: MetricDescriptor): Boolean =
    descriptor.sourceInstrument.instrumentType match {
      case InstrumentType.Counter           => true
      case InstrumentType.Histogram         => true
      case InstrumentType.ObservableCounter => true
      case _                                => false
    }

  private def makeReservoir: F[ExemplarReservoir[F, A, E]] =
    ExemplarReservoir
      .fixedSize[F, A, E](size = reservoirSize, traceContextLookup)
      .map(r => ExemplarReservoir.filtered(filter, r))

}

private object SumSynchronous {

  def apply[F[_]: Temporal: Random, A: MeasurementValue: Numeric](
      reservoirSize: Int,
      filter: ExemplarFilter,
      lookup: TraceContextLookup
  ): Aggregator.Synchronous[F, A] = {
    MeasurementValue[A] match {
      case MeasurementValue.LongMeasurementValue(_) =>
        new SumSynchronous(
          reservoirSize,
          filter,
          lookup,
          PointData.NumberPoint.Make.makeLong,
          ExemplarData.Make.makeLong
        )

      case MeasurementValue.DoubleMeasurementValue(_) =>
        new SumSynchronous(
          reservoirSize,
          filter,
          lookup,
          PointData.NumberPoint.Make.makeDouble,
          ExemplarData.Make.makeDouble
        )
    }
  }

  private class Accumulator[
      F[_]: Monad,
      A,
      P <: PointData.NumberPoint,
      E <: ExemplarData
  ](
      adder: Adder[F, A],
      reservoir: ExemplarReservoir[F, A, E],
      make: PointData.NumberPoint.Make[A, P, E]
  ) extends Aggregator.Accumulator[F, A, P] {

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
        make.make(
          startTimestamp,
          collectTimestamp,
          attributes,
          exemplars,
          value
        )
      )

    def record(value: A, attributes: Attributes, context: Context): F[Unit] =
      reservoir.offer(value, attributes, context) >> adder.add(value)

  }

}
