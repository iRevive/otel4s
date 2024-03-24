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
import org.typelevel.otel4s.sdk.metrics.data.ExemplarData.TraceContext
import org.typelevel.otel4s.sdk.metrics.data.PointData

import scala.concurrent.duration.FiniteDuration

private sealed trait Target[A] { self =>
  type Exemplar <: ExemplarData
  type Point <: PointData.NumberPoint

  def makePointData(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      exemplars: Vector[Exemplar],
      value: A
  ): Point

  def makeExemplar(
      attributes: Attributes,
      timestamp: FiniteDuration,
      traceContext: Option[TraceContext],
      value: A
  ): Exemplar

}

private object Target {

  def apply[A: MeasurementValue]: Target[A] =
    MeasurementValue[A] match {
      case MeasurementValue.LongMeasurementValue(cast) =>
        new Target[A] {
          type Exemplar = ExemplarData.LongExemplar
          type Point = PointData.LongNumber

          def makePointData(
              startTimestamp: FiniteDuration,
              collectTimestamp: FiniteDuration,
              attributes: Attributes,
              exemplars: Vector[ExemplarData.LongExemplar],
              value: A
          ): PointData.LongNumber =
            PointData.longNumber(
              startTimestamp,
              collectTimestamp,
              attributes,
              exemplars,
              cast(value)
            )

          def makeExemplar(
              attributes: Attributes,
              timestamp: FiniteDuration,
              traceContext: Option[TraceContext],
              value: A
          ): ExemplarData.LongExemplar =
            ExemplarData.long(attributes, timestamp, traceContext, cast(value))
        }

      case MeasurementValue.DoubleMeasurementValue(cast) =>
        new Target[A] {
          type Exemplar = ExemplarData.DoubleExemplar
          type Point = PointData.DoubleNumber

          def makePointData(
              startTimestamp: FiniteDuration,
              collectTimestamp: FiniteDuration,
              attributes: Attributes,
              exemplars: Vector[ExemplarData.DoubleExemplar],
              value: A
          ): PointData.DoubleNumber =
            PointData.doubleNumber(
              startTimestamp,
              collectTimestamp,
              attributes,
              exemplars,
              cast(value)
            )

          def makeExemplar(
              attributes: Attributes,
              timestamp: FiniteDuration,
              traceContext: Option[TraceContext],
              value: A
          ): ExemplarData.DoubleExemplar =
            ExemplarData.double(
              attributes,
              timestamp,
              traceContext,
              cast(value)
            )

        }

    }

}
