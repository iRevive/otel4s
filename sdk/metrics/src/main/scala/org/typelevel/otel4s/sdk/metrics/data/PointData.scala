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

import scala.concurrent.duration.FiniteDuration

/** A point in the metric data model.
  *
  * A point represents the aggregation of measurements recorded with a
  * particular set of [[Attributes]] over some time interval.
  */
sealed trait PointData {
  def startTimestamp: FiniteDuration
  def collectTimestamp: FiniteDuration
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
    def sum: Option[Double]
    def min: Option[Double]
    def max: Option[Double]
    def boundaries: Vector[Double]
    def counts: Vector[Long]
    def count: Long
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
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.LongExemplar],
      value: Long
  ): LongNumber =
    LongNumberImpl(
      startTimestamp,
      collectTimestamp,
      attributes,
      exemplars,
      value
    )

  def doubleNumber(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.DoubleExemplar],
      value: Double
  ): DoubleNumber =
    DoubleNumberImpl(
      startTimestamp,
      collectTimestamp,
      attributes,
      exemplars,
      value
    )

  def summary(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      count: Long,
      sum: Double,
      percentileValues: Vector[Summary.ValueAtQuantile]
  ): Summary =
    SummaryImpl(
      startTimestamp,
      collectTimestamp,
      attributes,
      count,
      sum,
      percentileValues
    )

  def histogram(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.DoubleExemplar],
      sum: Option[Double],
      min: Option[Double],
      max: Option[Double],
      boundaries: Vector[Double],
      counts: Vector[Long]
  ): Histogram = {
    require(counts.length == boundaries.size + 1)
    // todo require(isStrictlyIncreasing())

    HistogramImpl(
      startTimestamp,
      collectTimestamp,
      attributes,
      exemplars,
      sum,
      min,
      max,
      boundaries,
      counts
    )
  }

  def exponentialHistogram(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
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
      startTimestamp,
      collectTimestamp,
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
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.LongExemplar],
      value: Long
  ) extends LongNumber

  private final case class DoubleNumberImpl(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.DoubleExemplar],
      value: Double
  ) extends DoubleNumber

  private final case class SummaryImpl(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      count: Long,
      sum: Double,
      percentileValues: Vector[Summary.ValueAtQuantile]
  ) extends Summary

  private final case class HistogramImpl(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.DoubleExemplar],
      sum: Option[Double],
      min: Option[Double],
      max: Option[Double],
      boundaries: Vector[Double],
      counts: Vector[Long]
  ) extends Histogram {
    val count: Long = counts.sum
  }

  private final case class ExponentialHistogramImpl(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
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
