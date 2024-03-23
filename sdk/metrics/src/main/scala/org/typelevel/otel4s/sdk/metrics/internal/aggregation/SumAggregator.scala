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
import cats.effect.Temporal
import cats.effect.std.Random
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.metrics.ExemplarFilter
import org.typelevel.otel4s.sdk.metrics.InstrumentType
import org.typelevel.otel4s.sdk.metrics.data.AggregationTemporality
import org.typelevel.otel4s.sdk.metrics.data.Data
import org.typelevel.otel4s.sdk.metrics.data.ExemplarData
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.internal.exemplar.ExemplarReservoir
import org.typelevel.otel4s.sdk.metrics.internal.exemplar.TraceContextLookup
import org.typelevel.otel4s.sdk.metrics.internal.utils.Adder

import scala.concurrent.duration.FiniteDuration

object SumAggregator {

  def synchronous[F[_]: Temporal: Random, A: MeasurementValue: Numeric](
      reservoirSize: Int,
      filter: ExemplarFilter,
      lookup: TraceContextLookup
  ): Aggregator.Synchronous[F, A] =
    new Synchronous(reservoirSize, filter, lookup)

  def observable[
      F[_]: Applicative,
      A: MeasurementValue: Numeric
  ]: Aggregator.Observable[F, A] =
    new Observable[F, A]

  private final class Synchronous[
      F[_]: Temporal: Random,
      A: MeasurementValue: Numeric
  ](
      reservoirSize: Int,
      filter: ExemplarFilter,
      traceContextLookup: TraceContextLookup
  ) extends Aggregator.Synchronous[F, A] {

    val target: Target[A] = Target[A]

    private implicit val makeExemplar: ExemplarData.Make[A, target.Exemplar] =
      target.makeExemplar

    type Point = target.Point

    def createAccumulator: F[Aggregator.Accumulator[F, A, Point]] =
      for {
        adder <- Adder.create[F, A]
        reservoir <- makeReservoir
      } yield new Accumulator(adder, reservoir)

    def toMetricData(
        resource: TelemetryResource,
        scope: InstrumentationScope,
        descriptor: MetricDescriptor,
        points: Vector[Point],
        temporality: AggregationTemporality
    ): F[MetricData] =
      Temporal[F].pure(
        MetricData(
          resource,
          scope,
          descriptor.name,
          descriptor.description,
          descriptor.sourceInstrument.unit,
          Data.Sum(points, isMonotonic(descriptor), temporality)
        )
      )

    private def makeReservoir: F[ExemplarReservoir[F, A, target.Exemplar]] =
      ExemplarReservoir
        .fixedSize[F, A, target.Exemplar](
          size = reservoirSize,
          traceContextLookup
        )
        .map(r => ExemplarReservoir.filtered(filter, r))

    private class Accumulator(
        adder: Adder[F, A],
        reservoir: ExemplarReservoir[F, A, target.Exemplar]
    ) extends Aggregator.Accumulator[F, A, Point] {

      def aggregate(
          startTimestamp: FiniteDuration,
          collectTimestamp: FiniteDuration,
          attributes: Attributes,
          reset: Boolean
      ): F[Option[Point]] =
        for {
          value <- adder.sum(reset)
          exemplars <- reservoir.collectAndReset(attributes)
        } yield Some(
          target.makePointData(
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

  private final class Observable[
      F[_]: Applicative,
      A: MeasurementValue: Numeric
  ] extends Aggregator.Observable[F, A] {

    private val target: Target[A] = Target[A]

    def diff(
        previous: Measurement[A],
        current: Measurement[A]
    ): Measurement[A] =
      current.withValue(Numeric[A].minus(current.value, previous.value))

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
          Data.Sum(points, isMonotonic(descriptor), temporality)
        )
      )
    }

  }

  private def isMonotonic(descriptor: MetricDescriptor): Boolean =
    descriptor.sourceInstrument.instrumentType match {
      case InstrumentType.Counter           => true
      case InstrumentType.Histogram         => true
      case InstrumentType.ObservableCounter => true
      case _                                => false
    }

}
