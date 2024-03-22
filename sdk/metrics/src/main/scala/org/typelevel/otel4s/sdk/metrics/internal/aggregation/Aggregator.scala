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

import cats.Applicative
import cats.effect.Temporal
import cats.effect.std.Random
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.{BucketBoundaries, MeasurementValue}
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.metrics.data.AggregationTemporality
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.data.PointData
import org.typelevel.otel4s.sdk.metrics.internal.{
  InstrumentDescriptor,
  Measurement,
  MetricDescriptor
}
import org.typelevel.otel4s.sdk.metrics.internal.exemplar.TraceContextLookup

import scala.concurrent.duration.FiniteDuration

private[metrics] object Aggregator {

  trait Synchronous[F[_], A] {
    type Point <: PointData

    def createAccumulator: F[Aggregator.Accumulator[F, A, Point]]

    def toMetricData(
        resource: TelemetryResource,
        scope: InstrumentationScope,
        descriptor: MetricDescriptor,
        points: Vector[Point],
        temporality: AggregationTemporality
    ): F[MetricData]
  }

  trait Observable[F[_], A] {
    def diff(previous: Measurement[A], current: Measurement[A]): Measurement[A]

    def toMetricData(
        measurements: Vector[Measurement[A]],
        resource: TelemetryResource,
        scope: InstrumentationScope,
        descriptor: MetricDescriptor,
        temporality: AggregationTemporality
    ): F[MetricData]
  }

  type Aux[F[_], A, P <: PointData] = Aggregator.Synchronous[F, A] {
    type Point = P
  }

  trait Accumulator[F[_], A, P <: PointData] {
    def aggregate(
        startTimestamp: FiniteDuration,
        collectTimestamp: FiniteDuration,
        attributes: Attributes,
        reset: Boolean
    ): F[Option[P]]

    def record(
        value: A,
        attributes: Attributes,
        context: Context
    ): F[Unit]
  }

  def synchronous[F[_]: Temporal: Random, A: MeasurementValue: Numeric](
      aggregation: Aggregation.Synchronous,
      descriptor: InstrumentDescriptor.Synchronous,
      filter: ExemplarFilter,
      traceContextLookup: TraceContextLookup
  ): Aggregator.Synchronous[F, A] = {
    def sum: Aggregator.Synchronous[F, A] =
      SumSynchronous(
        Runtime.getRuntime.availableProcessors,
        filter,
        traceContextLookup
      )

    def lastValue: Aggregator.Synchronous[F, A] =
      LastValueSynchronous[F, A]

    def histogram: Aggregator.Synchronous[F, A] = {
      val boundaries =
        descriptor.advice.explicitBoundaries.getOrElse(BucketBoundaries.default)
      ExplicitBucketHistogram(boundaries, filter, traceContextLookup)
    }

    aggregation match {
      case Aggregation.Default =>
        descriptor.instrumentType match {
          case InstrumentType.Counter       => sum
          case InstrumentType.UpDownCounter => sum
          case InstrumentType.Histogram     => histogram
        }

      case Aggregation.Sum       => sum
      case Aggregation.LastValue => lastValue

      case Aggregation.ExplicitBucketHistogram(boundaries) =>
        ExplicitBucketHistogram(
          boundaries,
          filter,
          traceContextLookup
        )

      case Aggregation.Base2ExponentialHistogram(_, _) =>
        ???
    }
  }

  def observable[F[_]: Applicative, A: MeasurementValue: Numeric](
      aggregation: Aggregation.Observable,
      descriptor: InstrumentDescriptor.Observable
  ): Aggregator.Observable[F, A] = {
    def sum: Aggregator.Observable[F, A] =
      SumObservable[F, A]

    def lastValue: Aggregator.Observable[F, A] =
      LastValueObservable[F, A]

    aggregation match {
      case Aggregation.Default =>
        descriptor.instrumentType match {
          case InstrumentType.ObservableCounter       => sum
          case InstrumentType.ObservableUpDownCounter => sum
          case InstrumentType.ObservableGauge         => lastValue
        }

      case Aggregation.Sum       => sum
      case Aggregation.LastValue => lastValue
    }
  }

}
