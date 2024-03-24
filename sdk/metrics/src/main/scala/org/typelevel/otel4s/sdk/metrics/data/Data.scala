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

package org.typelevel.otel4s.sdk.metrics.data

/** A collection of metric data points.
  *
  * @see
  *   [[https://opentelemetry.io/docs/specs/otel/metrics/data-model/#metric-points]]
  */
sealed trait Data {

  /** The collection of the metric [[PointData]]s.
    */
  def points: Vector[PointData]
}

object Data {

  sealed trait Sum extends Data {
    type Point <: PointData.NumberPoint

    def points: Vector[Point]
    def isMonotonic: Boolean
    def aggregationTemporality: AggregationTemporality
  }

  sealed trait Gauge extends Data {
    type Point <: PointData.NumberPoint

    def points: Vector[Point]
  }

  sealed trait Summary extends Data {
    def points: Vector[PointData.Summary]
  }

  sealed trait Histogram extends Data {
    def points: Vector[PointData.Histogram]
    def aggregationTemporality: AggregationTemporality
  }

  sealed trait ExponentialHistogram extends Data {
    def points: Vector[PointData.ExponentialHistogram]
    def aggregationTemporality: AggregationTemporality
  }

  def sum[A <: PointData.NumberPoint](
      points: Vector[A],
      isMonotonic: Boolean,
      aggregationTemporality: AggregationTemporality
  ): Sum =
    SumImpl(points, isMonotonic, aggregationTemporality)

  def gauge[A <: PointData.NumberPoint](
      points: Vector[A]
  ): Gauge =
    GaugeImpl(points)

  def summary(
      points: Vector[PointData.Summary]
  ): Summary =
    SummaryImpl(points)

  def histogram(
      points: Vector[PointData.Histogram],
      aggregationTemporality: AggregationTemporality
  ): Histogram =
    HistogramImpl(points, aggregationTemporality)

  def exponentialHistogram(
      points: Vector[PointData.ExponentialHistogram],
      aggregationTemporality: AggregationTemporality
  ): ExponentialHistogram =
    ExponentialHistogramImpl(points, aggregationTemporality)

  private final case class SumImpl[A <: PointData.NumberPoint](
      points: Vector[A],
      isMonotonic: Boolean,
      aggregationTemporality: AggregationTemporality
  ) extends Sum { type Point = A }

  private final case class GaugeImpl[A <: PointData.NumberPoint](
      points: Vector[A]
  ) extends Gauge { type Point = A }

  private final case class SummaryImpl(
      points: Vector[PointData.Summary]
  ) extends Summary

  private final case class HistogramImpl(
      points: Vector[PointData.Histogram],
      aggregationTemporality: AggregationTemporality
  ) extends Histogram

  private final case class ExponentialHistogramImpl(
      points: Vector[PointData.ExponentialHistogram],
      aggregationTemporality: AggregationTemporality
  ) extends ExponentialHistogram

}
