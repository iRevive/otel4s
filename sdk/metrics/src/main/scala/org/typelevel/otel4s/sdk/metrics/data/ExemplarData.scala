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

sealed trait ExemplarData {

  def filteredAttributes: Attributes
  def timestamp: FiniteDuration
  // todo spanContext: SpanContext

}

object ExemplarData {

  def apply[A: MeasurementValue](
      filteredAttributes: Attributes,
      timestamp: FiniteDuration,
      value: A
  ): ExemplarData =
    MeasurementValue[A] match {
      case MeasurementValue.LongMeasurementValue(cast) =>
        LongExemplar(filteredAttributes, timestamp, cast(value))

      case MeasurementValue.DoubleMeasurementValue(cast) =>
        DoubleExemplar(filteredAttributes, timestamp, cast(value))
    }

  final case class LongExemplar(
      filteredAttributes: Attributes,
      timestamp: FiniteDuration,
      value: Long
  ) extends ExemplarData

  final case class DoubleExemplar(
      filteredAttributes: Attributes,
      timestamp: FiniteDuration,
      value: Double
  ) extends ExemplarData

}
