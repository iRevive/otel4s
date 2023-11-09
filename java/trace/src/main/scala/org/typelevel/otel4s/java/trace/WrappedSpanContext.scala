/*
 * Copyright 2022 Typelevel
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

package org.typelevel.otel4s.java.trace

import io.opentelemetry.api.trace.{SpanContext => JSpanContext}
import io.opentelemetry.api.trace.{TraceFlags => JTraceFlags}
import io.opentelemetry.api.trace.{TraceState => JTraceState}
import org.typelevel.otel4s.trace.SpanContext
import org.typelevel.otel4s.trace.TraceFlags
import org.typelevel.otel4s.trace.TraceState
import scodec.bits.ByteVector

private[otel4s] object WrappedSpanContext {

  def wrap(context: JSpanContext): SpanContext = {
    val entries = Vector.newBuilder[(String, String)]
    context.getTraceState.forEach((k, v) => entries.addOne(k -> v))
    val traceState = TraceState.fromVectorUnsafe(entries.result())

    SpanContext.delegate(
      underlying = context,
      traceId = ByteVector(context.getTraceIdBytes),
      spanId = ByteVector(context.getSpanIdBytes),
      traceFlags = TraceFlags.fromByte(context.getTraceFlags.asByte),
      traceState = traceState,
      remote = context.isRemote,
      isValid = context.isValid
    )
  }

  def unwrap(context: SpanContext): JSpanContext = {
    def flags = JTraceFlags.fromByte(context.traceFlags.toByte)

    def traceState =
      context.traceState.asMap
        .foldLeft(JTraceState.builder()) { case (builder, (key, value)) =>
          builder.put(key, value)
        }
        .build()

    context match {
      case ctx: SpanContext.Delegate[JSpanContext @unchecked] =>
        ctx.underlying

      case other if other.isRemote =>
        JSpanContext.createFromRemoteParent(
          other.traceIdHex,
          other.spanIdHex,
          flags,
          traceState
        )

      case other =>
        JSpanContext.create(
          other.traceIdHex,
          other.spanIdHex,
          flags,
          traceState
        )
    }
  }

}
