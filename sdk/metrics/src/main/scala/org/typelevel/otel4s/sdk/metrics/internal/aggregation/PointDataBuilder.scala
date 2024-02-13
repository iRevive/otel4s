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

package org.typelevel.otel4s.sdk.metrics.internal.aggregation

import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.metrics.data.ExemplarData
import org.typelevel.otel4s.sdk.metrics.data.PointData

import scala.concurrent.duration.FiniteDuration

private trait PointDataBuilder[A] {
  type Point <: PointData.NumberPoint
  type Exemplar <: ExemplarData

  def create(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      exemplars: Vector[Exemplar],
      value: A
  ): Point
}

private object PointDataBuilder {

  type Aux[A, P <: PointData.NumberPoint, E <: ExemplarData] =
    PointDataBuilder[A] {
      type Point = P
      type Exemplar = E
    }

  def apply[A: MeasurementValue]: PointDataBuilder[A] =
    MeasurementValue[A] match {
      case MeasurementValue.LongMeasurementValue(cast)   => ofLong(cast)
      case MeasurementValue.DoubleMeasurementValue(cast) => ofDouble(cast)
    }

  private def ofLong[A](cast: A => Long): PointDataBuilder[A] =
    new PointDataBuilder[A] {
      type Point = PointData.LongNumber
      type Exemplar = ExemplarData.LongExemplar

      def create(
          startTimestamp: FiniteDuration,
          collectTimestamp: FiniteDuration,
          attributes: Attributes,
          exemplars: Vector[ExemplarData.LongExemplar],
          value: A
      ): PointData.LongNumber =
        PointData.LongNumber(
          startTimestamp,
          collectTimestamp,
          attributes,
          exemplars,
          cast(value)
        )
    }

  private def ofDouble[A](cast: A => Double): PointDataBuilder[A] =
    new PointDataBuilder[A] {
      type Point = PointData.DoubleNumber
      type Exemplar = ExemplarData.DoubleExemplar

      def create(
          startTimestamp: FiniteDuration,
          collectTimestamp: FiniteDuration,
          attributes: Attributes,
          exemplars: Vector[ExemplarData.DoubleExemplar],
          value: A
      ): PointData.DoubleNumber =
        PointData.DoubleNumber(
          startTimestamp,
          collectTimestamp,
          attributes,
          exemplars,
          cast(value)
        )
    }

}
