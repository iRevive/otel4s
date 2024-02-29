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

package org.typelevel.otel4s.sdk.metrics.exporter

import cats.Applicative
import cats.Foldable
import org.typelevel.otel4s.sdk.metrics.data.MetricData

trait MetricExporter[F[_]] {

  def name: String

  def defaultAggregationSelector: DefaultAggregationSelector

  def aggregationTemporalitySelector: AggregationTemporalitySelector

  def exportMetrics[G[_]: Foldable](metrics: G[MetricData]): F[Unit]

  def flush: F[Unit]

  override def toString: String =
    name

}

object MetricExporter {

  def noop[F[_]: Applicative]: MetricExporter[F] =
    new MetricExporter[F] {
      def name: String = "Noop"
      def defaultAggregationSelector: DefaultAggregationSelector =
        DefaultAggregationSelector.default
      def aggregationTemporalitySelector: AggregationTemporalitySelector =
        AggregationTemporalitySelector.alwaysCumulative
      def exportMetrics[G[_]: Foldable](metrics: G[MetricData]): F[Unit] =
        Applicative[F].unit
      def flush: F[Unit] =
        Applicative[F].unit
    }

}
