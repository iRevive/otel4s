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
sealed trait MetricPoints {

  /** The collection of the metric [[PointData]]s.
    */
  def points: Vector[PointData]
}

object MetricPoints {

  /** Sum represents the type of a numeric double scalar metric that is
    * calculated as a sum of all reported measurements over a time interval.
    *
    * @see
    *   [[https://opentelemetry.io/docs/specs/otel/metrics/data-model/#sums]]
    */
  sealed trait Sum extends MetricPoints {
    type Point <: PointData.NumberPoint

    def points: Vector[Point]

    /** Whether the points are monotonic. If true, it means the data points are
      * nominally increasing.
      */
    def monotonic: Boolean

    /** The aggregation temporality of this aggregation.
      */
    def aggregationTemporality: AggregationTemporality
  }

  /** Gauge represents a sampled value at a given type.
    *
    * @see
    *   [[https://opentelemetry.io/docs/specs/otel/metrics/data-model/#gauge]]
    */
  sealed trait Gauge extends MetricPoints {
    type Point <: PointData.NumberPoint

    def points: Vector[Point]
  }

  sealed trait Summary extends MetricPoints {
    def points: Vector[PointData.Summary]
  }

  /** Histogram represents the type of a metric that is calculated by
    * aggregating as a histogram of all reported double measurements over a time
    * interval.
    *
    * @see
    *   [[https://opentelemetry.io/docs/specs/otel/metrics/data-model/#histogram]]
    */
  sealed trait Histogram extends MetricPoints {
    def points: Vector[PointData.Histogram]
    def aggregationTemporality: AggregationTemporality
  }

  sealed trait ExponentialHistogram extends MetricPoints {
    def points: Vector[PointData.ExponentialHistogram]
    def aggregationTemporality: AggregationTemporality
  }

  /** Creates a [[Sum]] with the given values.
    */
  def sum[A <: PointData.NumberPoint](
      points: Vector[A],
      monotonic: Boolean,
      aggregationTemporality: AggregationTemporality
  ): Sum =
    SumImpl(points, monotonic, aggregationTemporality)

  /** Creates a [[Gauge]] with the given values.
    */
  def gauge[A <: PointData.NumberPoint](
      points: Vector[A]
  ): Gauge =
    GaugeImpl(points)

  def summary(
      points: Vector[PointData.Summary]
  ): Summary =
    SummaryImpl(points)

  /** Creates a [[Histogram]] with the given values.
    */
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
      monotonic: Boolean,
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
