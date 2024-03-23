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
import scodec.bits.ByteVector

import scala.concurrent.duration.FiniteDuration

sealed trait ExemplarData {
  def filteredAttributes: Attributes
  def timestamp: FiniteDuration
  def traceContext: Option[ExemplarData.TraceContext]
}

object ExemplarData {

  sealed trait TraceContext {
    def traceId: ByteVector
    def spanId: ByteVector
  }

  object TraceContext {

    def apply(traceId: ByteVector, spanId: ByteVector): TraceContext =
      Impl(traceId, spanId)

    private final case class Impl(
        traceId: ByteVector,
        spanId: ByteVector
    ) extends TraceContext
  }

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
