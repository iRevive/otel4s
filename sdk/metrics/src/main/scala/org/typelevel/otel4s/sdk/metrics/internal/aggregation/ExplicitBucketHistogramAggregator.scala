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

import cats.FlatMap
import cats.effect.Concurrent
import cats.effect.Ref
import cats.effect.Temporal
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.BucketBoundaries
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
import org.typelevel.otel4s.sdk.metrics.internal.MetricDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.exemplar.ExemplarReservoir

import scala.concurrent.duration.FiniteDuration

private final class ExplicitBucketHistogramAggregator[
    F[_]: Concurrent,
    A: MeasurementValue
](
    boundaries: BucketBoundaries,
    makeReservoir: F[ExemplarReservoir[F, A, ExemplarData.DoubleExemplar]]
) extends Aggregator[F, A] {
  import ExplicitBucketHistogramAggregator._

  type Point = PointData.Histogram

  def createHandle: F[Aggregator.Handle[F, A, PointData.Histogram]] =
    for {
      state <- Concurrent[F].ref(emptyState(boundaries.length))
      reservoir <- makeReservoir
    } yield new Handle(state, boundaries, reservoir)

  def toMetricData(
      resource: TelemetryResource,
      scope: InstrumentationScope,
      descriptor: MetricDescriptor,
      points: Vector[PointData.Histogram],
      temporality: AggregationTemporality
  ): F[MetricData] =
    Concurrent[F].pure(
      MetricData(
        resource,
        scope,
        descriptor.name,
        descriptor.description,
        descriptor.sourceInstrument.unit,
        Data.Histogram(points, temporality)
      )
    )
}

private object ExplicitBucketHistogramAggregator {

  def apply[F[_]: Temporal, A: MeasurementValue: Numeric](
      boundaries: BucketBoundaries,
      filter: ExemplarFilter
  ): ExplicitBucketHistogramAggregator[F, A] = {
    val reservoir = ExemplarReservoir
      .histogramBucket[F, A, ExemplarData.DoubleExemplar](boundaries)
      .map(r => ExemplarReservoir.filtered(filter, r))

    new ExplicitBucketHistogramAggregator[F, A](boundaries, reservoir)
  }

  private final case class State(
      sum: Double,
      min: Double,
      max: Double,
      count: Long,
      counts: Vector[Long] // todo use array for memory efficiency?
  )

  private def emptyState(counts: Int): State =
    State(0, Double.MaxValue, -1, 0L, Vector.fill(counts)(0))

  private class Handle[F[_]: FlatMap, A: MeasurementValue](
      stateRef: Ref[F, State],
      boundaries: BucketBoundaries,
      reservoir: ExemplarReservoir[F, A, ExemplarData.DoubleExemplar]
  ) extends Aggregator.Handle[F, A, PointData.Histogram] {

    def aggregate(
        startTimestamp: FiniteDuration,
        collectTimestamp: FiniteDuration,
        attributes: Attributes,
        reset: Boolean
    ): F[Option[PointData.Histogram]] =
      reservoir.collectAndReset(attributes).flatMap { exemplars =>
        stateRef.modify { state =>
          val nonEmpty = state.count > 0
          val histogram = PointData.Histogram(
            startTimestamp = startTimestamp,
            collectTimestamp = collectTimestamp,
            attributes = attributes,
            exemplars = exemplars,
            sum = Option.when(nonEmpty)(state.sum),
            min = Option.when(nonEmpty)(state.min),
            max = Option.when(nonEmpty)(state.max),
            boundaries = boundaries.boundaries,
            counts = state.counts
          )

          val next = if (reset) emptyState(boundaries.length) else state

          (next, Some(histogram))
        }
      }

    def record(value: A, attributes: Attributes, context: Context): F[Unit] = {
      val doubleValue = MeasurementValue[A].toDouble(value)

      reservoir.offer(value, attributes, context) >> stateRef.update { state =>
        val idx = boundaries.bucketIndex(doubleValue)
        state.copy(
          sum = state.sum + doubleValue,
          min = math.min(state.min, doubleValue),
          max = math.max(state.max, doubleValue),
          count = state.count + 1,
          counts = state.counts.updated(idx, state.counts(idx) + 1)
        )
      }
    }

  }

}
