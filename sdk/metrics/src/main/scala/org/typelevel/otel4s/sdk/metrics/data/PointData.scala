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
import org.typelevel.otel4s.metrics.MeasurementValue

import scala.concurrent.duration.FiniteDuration

sealed trait PointData {
  def startTimestamp: FiniteDuration
  def collectTimestamp: FiniteDuration
  def attributes: Attributes
}

object PointData {

  sealed trait NumberPoint extends PointData {
    def exemplars: Vector[ExemplarData]
  }

  final case class LongNumber(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.LongExemplar],
      value: Long
  ) extends NumberPoint

  final case class DoubleNumber(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.DoubleExemplar],
      value: Double
  ) extends NumberPoint

  final case class Summary(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      count: Long,
      sum: Double,
      percentileValues: Vector[ValueAtQuantile]
  ) extends PointData

  final case class Histogram(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      exemplars: Vector[ExemplarData.DoubleExemplar],
      sum: Option[Double],
      min: Option[Double],
      max: Option[Double],
      boundaries: Vector[Double],
      counts: Vector[Long]
  ) extends PointData {
    require(counts.length == boundaries.size + 1)
    // todo require(isStrictlyIncreasing())

    val count: Long = counts.sum
  }

  final case class ExponentialHistogram(
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
      positiveBuckets: ExponentialHistogramBuckets,
      negativeBuckets: ExponentialHistogramBuckets
  ) extends PointData {
    val count: Long =
      zeroCount + positiveBuckets.totalCount + negativeBuckets.totalCount
  }

  object NumberPoint {

    private[metrics] sealed trait Make[A, P <: NumberPoint, E <: ExemplarData] {
      def make(
          startTimestamp: FiniteDuration,
          collectTimestamp: FiniteDuration,
          attributes: Attributes,
          exemplars: Vector[E],
          value: A
      ): P
    }

    private[metrics] object Make {

      def makeLong[A: MeasurementValue]: Make[
        A,
        LongNumber,
        ExemplarData.LongExemplar
      ] =
        new Make[A, LongNumber, ExemplarData.LongExemplar] {
          def make(
              startTimestamp: FiniteDuration,
              collectTimestamp: FiniteDuration,
              attributes: Attributes,
              exemplars: Vector[ExemplarData.LongExemplar],
              value: A
          ): LongNumber =
            LongNumber(
              startTimestamp,
              collectTimestamp,
              attributes,
              exemplars,
              MeasurementValue[A].toLong(value)
            )
        }

      def makeDouble[A: MeasurementValue]: Make[
        A,
        DoubleNumber,
        ExemplarData.DoubleExemplar
      ] =
        new Make[A, DoubleNumber, ExemplarData.DoubleExemplar] {
          def make(
              startTimestamp: FiniteDuration,
              collectTimestamp: FiniteDuration,
              attributes: Attributes,
              exemplars: Vector[ExemplarData.DoubleExemplar],
              value: A
          ): DoubleNumber =
            DoubleNumber(
              startTimestamp,
              collectTimestamp,
              attributes,
              exemplars,
              MeasurementValue[A].toDouble(value)
            )
        }

    }

  }

}
