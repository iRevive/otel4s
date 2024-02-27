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
  */
sealed abstract class Data {

  /** The collection of the metric [[PointData]]s.
    */
  def points: Vector[PointData]
}

object Data {

  final case class Sum[A <: PointData.NumberPoint](
      points: Vector[A],
      isMonotonic: Boolean,
      aggregationTemporality: AggregationTemporality
  ) extends Data

  final case class Gauge[A <: PointData.NumberPoint](
      points: Vector[A]
  ) extends Data

  final case class Summary(
      points: Vector[PointData.Summary]
  ) extends Data

  final case class Histogram(
      points: Vector[PointData.Histogram],
      aggregationTemporality: AggregationTemporality
  ) extends Data

  final case class ExponentialHistogram(
      points: Vector[PointData.ExponentialHistogram],
      aggregationTemporality: AggregationTemporality
  ) extends Data

}
