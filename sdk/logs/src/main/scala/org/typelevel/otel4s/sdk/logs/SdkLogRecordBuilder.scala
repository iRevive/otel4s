package org.typelevel.otel4s.sdk.logs

import java.time.Instant

import cats.Monad
import cats.effect.Clock
import cats.mtl.Ask
import cats.syntax.all._
import org.typelevel.otel4s.{Attribute, Attributes, Value}
import org.typelevel.otel4s.logs.{LogRecordBuilder, Severity}
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.context.{AskContext, Context}
import org.typelevel.otel4s.sdk.logs.data.LogRecordData
import org.typelevel.otel4s.sdk.logs.processor.LogRecordProcessor

import scala.collection.immutable
import scala.concurrent.duration._

private final case class SdkLogRecordBuilder[F[_]: Monad: Clock: AskContext](
    processor: LogRecordProcessor[F],
    instrumentationScope: InstrumentationScope,
    resource: TelemetryResource,
    state: SdkLogRecordBuilder.State
) extends LogRecordBuilder[F] {

  def withTimestamp(timestamp: FiniteDuration): LogRecordBuilder[F] =
    copy(state = state.copy(timestamp = Some(timestamp)))

  def withTimestamp(timestamp: Instant): LogRecordBuilder[F] =
    copy(state = state.copy(timestamp = Some(timestamp.toEpochMilli.millis)))

  def withObservedTimestamp(timestamp: FiniteDuration): LogRecordBuilder[F] =
    copy(state = state.copy(observedTimestamp = Some(timestamp)))

  def withObservedTimestamp(timestamp: Instant): LogRecordBuilder[F] =
    copy(state = state.copy(observedTimestamp = Some(timestamp.toEpochMilli.millis)))

  /*def withTraceContext(context: TraceContext): LogRecordBuilder[F] =
    copy(state = state.copy(timestamp = Some(timestamp)))*/

  def withSeverity(severity: Severity): LogRecordBuilder[F] =
    copy(state = state.copy(severity = Some(severity)))

  def withSeverityText(severityText: String): LogRecordBuilder[F] =
    copy(state = state.copy(severityText = Some(severityText)))

  def withBody(body: Value): LogRecordBuilder[F] =
    copy(state = state.copy(body = Some(body)))

  def addAttribute[A](attribute: Attribute[A]): LogRecordBuilder[F] =
    copy(state = state.copy(attributes = state.attributes + attribute))

  def addAttributes(attributes: Attribute[_]*): LogRecordBuilder[F] =
    copy(state = state.copy(attributes = state.attributes ++ attributes))

  def addAttributes(attributes: immutable.Iterable[Attribute[_]]): LogRecordBuilder[F] =
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
      traceContext = None, // todo
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
  ): SdkLogRecordBuilder[F] =
    SdkLogRecordBuilder(processor, instrumentationScope, resource, EmptyState)

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
