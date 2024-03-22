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

import cats.Hash
import cats.Show
import cats.syntax.show._
import org.typelevel.otel4s.metrics.BucketBoundaries

/** The aggregation strategy for measurements.
  *
  * @param supportedInstruments
  *   the set of supported instruments
  */
sealed abstract class Aggregation(
    supportedInstruments: Set[InstrumentType]
) {
  def compatibleWith(tpe: InstrumentType): Boolean =
    supportedInstruments.contains(tpe)
}

object Aggregation {

  /** Drops all measurements and doesn't export any metric.
    */
  def drop: Aggregation = Drop

  /** Chooses an aggregator based on the [[InstrumentType]]:
    *   - counter - [[sum]]
    *   - up down counter - [[sum]]
    *   - observable counter - [[sum]]
    *   - observable up down counter - [[sum]]
    *   - histogram - `explicitBucketHistogram`
    *   - observable gauge - [[lastValue]]
    */
  def default: Aggregation = Default

  /** Aggregates measurements into
    * [[org.typelevel.otel4s.sdk.metrics.data.Data.Sum Data.Sum]].
    *
    * Compatible instruments:
    *   - [[org.typelevel.otel4s.metrics.Counter Counter]]
    *   - [[org.typelevel.otel4s.metrics.UpDownCounter UpDownCounter]]
    *   - [[org.typelevel.otel4s.metrics.Histogram Histogram]]
    *   - [[org.typelevel.otel4s.metrics.ObservableGauge ObservableGauge]]
    *   - [[org.typelevel.otel4s.metrics.ObservableCounter ObservableCounter]]
    */
  def sum: Aggregation = Sum

  /** Aggregates measurements into
    * [[org.typelevel.otel4s.sdk.metrics.data.Data.Gauge Data.Gauge]] using the
    * last seen measurement.
    *
    * Compatible instruments:
    *   - [[org.typelevel.otel4s.metrics.ObservableGauge ObservableGauge]]
    */
  def lastValue: Aggregation = LastValue

  /** Aggregates measurements into an explicit bucket
    * [[org.typelevel.otel4s.sdk.metrics.data.Data.Histogram Data.Histogram]]
    * using the default bucket boundaries.
    *
    * Compatible instruments:
    *   - [[org.typelevel.otel4s.metrics.Counter Counter]]
    *   - [[org.typelevel.otel4s.metrics.Histogram Histogram]]
    *
    * @see
    *   [[org.typelevel.otel4s.metrics.BucketBoundaries.default]] - default
    *   bucket boundaries
    */
  def explicitBucketHistogram: Aggregation =
    ExplicitBucketHistogram(BucketBoundaries.default)

  /** Aggregates measurements into an explicit bucket
    * [[org.typelevel.otel4s.sdk.metrics.data.Data.Histogram Data.Histogram]]
    * using the given bucket boundaries.
    *
    * Compatible instruments:
    *   - [[org.typelevel.otel4s.metrics.Counter Counter]]
    *   - [[org.typelevel.otel4s.metrics.Histogram Histogram]]
    *
    * @param boundaries
    *   the boundaries to use
    */
  def explicitBucketHistogram(boundaries: BucketBoundaries): Aggregation =
    ExplicitBucketHistogram(boundaries)

  /** Aggregates measurements into a base-2
    * [[org.typelevel.otel4s.sdk.metrics.data.Data.ExponentialHistogram Data.ExponentialHistogram]]
    * using the default `maxBuckets` and `maxScale`.
    *
    * Compatible instruments:
    *   - [[org.typelevel.otel4s.metrics.Counter Counter]]
    *   - [[org.typelevel.otel4s.metrics.Histogram Histogram]]
    */
  def base2ExponentialBucketHistogram: Aggregation =
    Base2ExponentialHistogram(160, 20) // todo: use const variables

  /** Aggregates measurements into a base-2
    * [[org.typelevel.otel4s.sdk.metrics.data.Data.ExponentialHistogram Data.ExponentialHistogram]]
    * using the given `maxBuckets` and `maxScale`.
    *
    * Compatible instruments:
    *   - [[org.typelevel.otel4s.metrics.Counter Counter]]
    *   - [[org.typelevel.otel4s.metrics.Histogram Histogram]]
    *
    * @param maxBuckets
    *   the max number of positive and negative buckets
    *
    * @param maxScale
    *   the maximum and initial scale
    */
  def base2ExponentialBucketHistogram(
      maxBuckets: Int,
      maxScale: Int
  ): Aggregation =
    Base2ExponentialHistogram(maxBuckets, maxScale)

  implicit val aggregationHash: Hash[Aggregation] =
    Hash.fromUniversalHashCode

  implicit val aggregationShow: Show[Aggregation] =
    Show.show {
      case Drop      => "Aggregation.Drop"
      case Default   => "Aggregation.Default"
      case Sum       => "Aggregation.Sum"
      case LastValue => "Aggregation.LastValue"
      case ExplicitBucketHistogram(boundaries) =>
        show"Aggregation.ExplicitBucketHistogram{boundaries=$boundaries}"
      case Base2ExponentialHistogram(maxBuckets, maxScale) =>
        show"Aggregation.Base2ExponentialHistogram{maxBuckets=$maxBuckets, maxScale=$maxScale}"
    }

  private[metrics] sealed trait Synchronous { self: Aggregation => }
  private[metrics] sealed trait Observable { self: Aggregation => }

  private[metrics] case object Drop extends Aggregation(Compatability.Drop)

  private[metrics] case object Default
      extends Aggregation(Compatability.Default)
      with Synchronous
      with Observable

  private[metrics] case object Sum
      extends Aggregation(Compatability.Sum)
      with Synchronous
      with Observable

  private[metrics] case object LastValue
      extends Aggregation(Compatability.LastValue)
      with Synchronous
      with Observable

  private[metrics] final case class ExplicitBucketHistogram(
      boundaries: BucketBoundaries
  ) extends Aggregation(Compatability.ExplicitBucketHistogram)
      with Synchronous

  private[metrics] final case class Base2ExponentialHistogram(
      maxBuckets: Int,
      maxScale: Int
  ) extends Aggregation(Compatability.Base2ExponentialHistogram)
      with Synchronous

  private object Compatability {
    val Drop: Set[InstrumentType] =
      InstrumentType.values

    val Default: Set[InstrumentType] =
      InstrumentType.values

    val Sum: Set[InstrumentType] = Set(
      InstrumentType.Counter,
      InstrumentType.UpDownCounter,
      InstrumentType.ObservableGauge,
      InstrumentType.ObservableUpDownCounter,
      InstrumentType.Histogram
    )

    val LastValue: Set[InstrumentType] =
      Set(InstrumentType.ObservableGauge)

    val ExplicitBucketHistogram: Set[InstrumentType] =
      Set(InstrumentType.Counter, InstrumentType.Histogram)

    val Base2ExponentialHistogram: Set[InstrumentType] =
      Set(InstrumentType.Counter, InstrumentType.Histogram)
  }

}
