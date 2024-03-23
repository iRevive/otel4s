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
import scodec.bits.ByteVector

import scala.concurrent.duration.FiniteDuration

sealed trait ExemplarData {
  type Value

  def filteredAttributes: Attributes
  def timestamp: FiniteDuration
  def traceContext: Option[ExemplarData.TraceContext]
  def value: Value
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

  sealed trait LongExemplar extends ExemplarData { type Value = Long }

  sealed trait DoubleExemplar extends ExemplarData { type Value = Double }

  def long(
      filteredAttributes: Attributes,
      timestamp: FiniteDuration,
      traceContext: Option[TraceContext],
      value: Long
  ): LongExemplar =
    LongExemplarImpl(filteredAttributes, timestamp, traceContext, value)

  def double(
      filteredAttributes: Attributes,
      timestamp: FiniteDuration,
      traceContext: Option[TraceContext],
      value: Double
  ): DoubleExemplar =
    DoubleExemplarImpl(filteredAttributes, timestamp, traceContext, value)

  private final case class LongExemplarImpl(
      filteredAttributes: Attributes,
      timestamp: FiniteDuration,
      traceContext: Option[TraceContext],
      value: Long
  ) extends LongExemplar

  private final case class DoubleExemplarImpl(
      filteredAttributes: Attributes,
      timestamp: FiniteDuration,
      traceContext: Option[TraceContext],
      value: Double
  ) extends DoubleExemplar

}
