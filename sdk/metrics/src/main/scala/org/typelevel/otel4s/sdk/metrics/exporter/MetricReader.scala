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
package exporter

import cats.data.NonEmptyVector
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.std.Console
import org.typelevel.otel4s.sdk.metrics.data.MetricData

import scala.concurrent.duration.FiniteDuration

trait MetricReader[F[_]] {
  def aggregationTemporalitySelector: AggregationTemporalitySelector
  def defaultAggregationSelector: AggregationSelector
  def defaultCardinalityLimitSelector: CardinalityLimitSelector
  def register(producers: NonEmptyVector[MetricProducer[F]]): F[Unit]
  def collectAllMetrics: F[Vector[MetricData]]
  def forceFlush: F[Unit]
}

object MetricReader {

  def periodic[F[_]: Temporal: Console](
      exporter: MetricExporter[F],
      interval: FiniteDuration
  ): Resource[F, MetricReader[F]] =
    PeriodicMetricReader.create(exporter, interval)

}
