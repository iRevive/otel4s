/*
 * Copyright 2025 Typelevel
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

package org.typelevel.otel4s.sdk.logs

import cats.Monad
import cats.effect.Clock
import cats.mtl.Ask
import cats.syntax.all._
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.Value
import org.typelevel.otel4s.logs.LogRecordBuilder
import org.typelevel.otel4s.logs.Severity
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.context.AskContext
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.context.TraceContext
import org.typelevel.otel4s.sdk.logs.data.LogRecordData
import org.typelevel.otel4s.sdk.logs.processor.LogRecordProcessor

import java.time.Instant
import scala.collection.immutable
import scala.concurrent.duration._

private final case class SdkLogRecordBuilder[F[_]: Monad: Clock: AskContext](
    processor: LogRecordProcessor[F],
    instrumentationScope: InstrumentationScope,
    resource: TelemetryResource,
    traceContextLookup: TraceContext.Lookup,
    state: SdkLogRecordBuilder.State
) extends LogRecordBuilder[F, Context] {

  def withTimestamp(timestamp: FiniteDuration): LogRecordBuilder[F, Context] =
    copy(state = state.copy(timestamp = Some(timestamp)))

  def withTimestamp(timestamp: Instant): LogRecordBuilder[F, Context] =
    copy(state = state.copy(timestamp = Some(timestamp.toEpochMilli.millis)))

  def withObservedTimestamp(timestamp: FiniteDuration): LogRecordBuilder[F, Context] =
    copy(state = state.copy(observedTimestamp = Some(timestamp)))

  def withObservedTimestamp(timestamp: Instant): LogRecordBuilder[F, Context] =
    copy(state = state.copy(observedTimestamp = Some(timestamp.toEpochMilli.millis)))

  def withContext(context: Context): LogRecordBuilder[F, Context] =
    copy(state = state.copy(context = Some(context)))

  def withSeverity(severity: Severity): LogRecordBuilder[F, Context] =
    copy(state = state.copy(severity = Some(severity)))

  def withSeverityText(severityText: String): LogRecordBuilder[F, Context] =
    copy(state = state.copy(severityText = Some(severityText)))

  def withBody(body: Value): LogRecordBuilder[F, Context] =
    copy(state = state.copy(body = Some(body)))

  def addAttribute[A](attribute: Attribute[A]): LogRecordBuilder[F, Context] =
    copy(state = state.copy(attributes = state.attributes + attribute))

  def addAttributes(attributes: Attribute[_]*): LogRecordBuilder[F, Context] =
    copy(state = state.copy(attributes = state.attributes ++ attributes))

  def addAttributes(attributes: immutable.Iterable[Attribute[_]]): LogRecordBuilder[F, Context] =
    copy(state = state.copy(attributes = state.attributes ++ attributes))

  def emit: F[Unit] =
    for {
      context <- Ask[F, Context].ask
      observedTimestamp <- state.observedTimestamp.fold(Clock[F].realTime)(Monad[F].pure(_))
      _ <- processor.onEmit(context, toLogRecord(observedTimestamp))
    } yield ()

  private def toLogRecord(observedTimestamp: FiniteDuration): LogRecordData =
    LogRecordData(
      timestamp = state.timestamp,
      observedTimestamp = observedTimestamp,
      traceContext = state.context.flatMap(ctx => traceContextLookup.get(ctx)),
      severity = state.severity,
      severityText = state.severityText,
      body = state.body,
      attributes = state.attributes,
      instrumentationScope = instrumentationScope,
      resource = resource
    )
}

private object SdkLogRecordBuilder {

  private val EmptyState = State(
    timestamp = None,
    observedTimestamp = None,
    context = None,
    severity = None,
    severityText = None,
    body = None,
    attributes = Attributes.empty
  )

  def empty[F[_]: Monad: Clock: AskContext](
      processor: LogRecordProcessor[F],
      instrumentationScope: InstrumentationScope,
      resource: TelemetryResource,
      traceContextLookup: TraceContext.Lookup
  ): SdkLogRecordBuilder[F] =
    SdkLogRecordBuilder(processor, instrumentationScope, resource, traceContextLookup, EmptyState)

  private[SdkLogRecordBuilder] final case class State(
      timestamp: Option[FiniteDuration],
      observedTimestamp: Option[FiniteDuration],
      context: Option[Context],
      severity: Option[Severity],
      severityText: Option[String],
      body: Option[Value],
      attributes: Attributes
  )

}
