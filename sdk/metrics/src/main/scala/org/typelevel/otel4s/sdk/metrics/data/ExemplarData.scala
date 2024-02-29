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
  def traceContext: Option[ExemplarData.TraceContext]
}

object ExemplarData {

  sealed trait TraceContext {
    def traceIdHex: String
    def spanIdHex: String
  }

  object TraceContext {

    def apply(traceIdHex: String, spanIdHex: String): TraceContext =
      Impl(traceIdHex, spanIdHex)

    private final case class Impl(
        traceIdHex: String,
        spanIdHex: String
    ) extends TraceContext
  }

  /*type Aux[A] = ExemplarData { type Value = A }

  def apply[A: MeasurementValue, E: MeasurementValue](
      attributes: Attributes,
      timestamp: FiniteDuration,
      spanContext: Option[SpanContext],
      value: A
  ): ExemplarData.Aux[E] =
    MeasurementValue[E] match {
      case MeasurementValue.LongMeasurementValue(_)   =>
        LongExemplar(attributes, timestamp, spanContext, MeasurementValue[A].toLong(value))

      case MeasurementValue.DoubleMeasurementValue(_) =>
        DoubleExemplar(attributes, timestamp, spanContext, MeasurementValue[A].toDouble(value))
    }*/

  final case class LongExemplar(
      filteredAttributes: Attributes,
      timestamp: FiniteDuration,
      traceContext: Option[TraceContext],
      value: Long
  ) extends ExemplarData

  final case class DoubleExemplar(
      filteredAttributes: Attributes,
      timestamp: FiniteDuration,
      traceContext: Option[TraceContext],
      value: Double
  ) extends ExemplarData

  private[metrics] sealed trait Make[A, E <: ExemplarData] {
    def make(
        attributes: Attributes,
        timestamp: FiniteDuration,
        traceContext: Option[TraceContext],
        value: A
    ): E
  }

  private[metrics] object Make {

    implicit def makeLong[A: MeasurementValue]: Make[A, LongExemplar] =
      new Make[A, LongExemplar] {
        def make(
            attributes: Attributes,
            timestamp: FiniteDuration,
            traceContext: Option[TraceContext],
            value: A
        ): LongExemplar =
          ExemplarData.LongExemplar(
            attributes,
            timestamp,
            traceContext,
            MeasurementValue[A].toLong(value)
          )
      }

    implicit def makeDouble[A: MeasurementValue]: Make[A, DoubleExemplar] =
      new Make[A, DoubleExemplar] {
        def make(
            attributes: Attributes,
            timestamp: FiniteDuration,
            traceContext: Option[TraceContext],
            value: A
        ): DoubleExemplar =
          ExemplarData.DoubleExemplar(
            attributes,
            timestamp,
            traceContext,
            MeasurementValue[A].toDouble(value)
          )
      }

  }

}
