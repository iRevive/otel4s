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

import org.typelevel.otel4s.Attributes

/** A point in the metric data model.
  *
  * A point represents the aggregation of measurements recorded with a
  * particular set of [[Attributes]] over some time interval.
  */
sealed trait PointData {

  /** A [[TimeWindow]] for which the point data was calculated.
    */
  def timeWindow: TimeWindow
  def attributes: Attributes
}

object PointData {

  sealed trait NumberPoint extends PointData {
    type Exemplar <: ExemplarData
    type Value

    def exemplars: Vector[Exemplar]
    def value: Value
  }

  sealed trait LongNumber extends NumberPoint {
    type Exemplar = ExemplarData.LongExemplar
    type Value = Long
  }

  sealed trait DoubleNumber extends NumberPoint {
    type Exemplar = ExemplarData.DoubleExemplar
    type Value = Double
  }

  sealed trait Summary extends PointData {
    def count: Long
    def sum: Double
    def percentileValues: Vector[Summary.ValueAtQuantile]
  }

  object Summary {
    sealed trait ValueAtQuantile {
      def quantile: Double
      def value: Double
    }
  }

  sealed trait Histogram extends PointData {
    def exemplars: Vector[ExemplarData.DoubleExemplar]
    def stats: Option[Histogram.Stats]
    def boundaries: Vector[Double]
    def counts: Vector[Long]
  }

  object Histogram {
    sealed trait Stats {
      def sum: Double
      def min: Double
      def max: Double
      def count: Long
    }

    def stats(sum: Double, min: Double, max: Double, count: Long): Stats =
      StatsImpl(sum, min, max, count)

    private final case class StatsImpl(
        sum: Double,
        min: Double,
        max: Double,
        count: Long
    ) extends Stats
  }

  sealed trait ExponentialHistogram extends PointData {
    def exemplars: Vector[ExemplarData.DoubleExemplar]
    def sum: Double
    def zeroCount: Long
    def hasMin: Boolean
    def min: Double
    def hasMax: Boolean
    def max: Double
    val count: Long
    def positiveBuckets: ExponentialHistogram.Buckets
    def negativeBuckets: ExponentialHistogram.Buckets
  }

  object ExponentialHistogram {
    sealed trait Buckets {
      def scale: Int
      def offset: Int
      def bucketCounts: Vector[Long]
      def totalCount: Long
    }
  }

  def longNumber(
      timeWindow: TimeWindow,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.LongExemplar],
      value: Long
  ): LongNumber =
    LongNumberImpl(
      timeWindow,
      attributes,
      exemplars,
      value
    )

  def doubleNumber(
      timeWindow: TimeWindow,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.DoubleExemplar],
      value: Double
  ): DoubleNumber =
    DoubleNumberImpl(
      timeWindow,
      attributes,
      exemplars,
      value
    )

  def summary(
      timeWindow: TimeWindow,
      attributes: Attributes,
      count: Long,
      sum: Double,
      percentileValues: Vector[Summary.ValueAtQuantile]
  ): Summary =
    SummaryImpl(
      timeWindow,
      attributes,
      count,
      sum,
      percentileValues
    )

  def histogram(
      timeWindow: TimeWindow,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.DoubleExemplar],
      stats: Option[Histogram.Stats],
      boundaries: Vector[Double],
      counts: Vector[Long]
  ): Histogram = {
    require(counts.length == boundaries.size + 1)
    // todo require(isStrictlyIncreasing())

    HistogramImpl(
      timeWindow,
      attributes,
      exemplars,
      stats,
      boundaries,
      counts
    )
  }

  def exponentialHistogram(
      timeWindow: TimeWindow,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.DoubleExemplar],
      sum: Double,
      zeroCount: Long,
      hasMin: Boolean,
      min: Double,
      hasMax: Boolean,
      max: Double,
      positiveBuckets: ExponentialHistogram.Buckets,
      negativeBuckets: ExponentialHistogram.Buckets
  ): ExponentialHistogram =
    ExponentialHistogramImpl(
      timeWindow,
      attributes,
      exemplars,
      sum,
      zeroCount,
      hasMin,
      min,
      hasMax,
      max,
      positiveBuckets,
      negativeBuckets
    )

  private final case class LongNumberImpl(
      timeWindow: TimeWindow,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.LongExemplar],
      value: Long
  ) extends LongNumber

  private final case class DoubleNumberImpl(
      timeWindow: TimeWindow,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.DoubleExemplar],
      value: Double
  ) extends DoubleNumber

  private final case class SummaryImpl(
      timeWindow: TimeWindow,
      attributes: Attributes,
      count: Long,
      sum: Double,
      percentileValues: Vector[Summary.ValueAtQuantile]
  ) extends Summary

  private final case class HistogramImpl(
      timeWindow: TimeWindow,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.DoubleExemplar],
      stats: Option[Histogram.Stats],
      boundaries: Vector[Double],
      counts: Vector[Long]
  ) extends Histogram

  private final case class ExponentialHistogramImpl(
      timeWindow: TimeWindow,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.DoubleExemplar],
      sum: Double,
      zeroCount: Long,
      hasMin: Boolean,
      min: Double,
      hasMax: Boolean,
      max: Double,
      positiveBuckets: ExponentialHistogram.Buckets,
      negativeBuckets: ExponentialHistogram.Buckets
  ) extends ExponentialHistogram {
    val count: Long =
      zeroCount + positiveBuckets.totalCount + negativeBuckets.totalCount
  }

}
