package org.typelevel.otel4s.sdk.logs.data

import cats.{Hash, Show}
import cats.syntax.show._
import org.typelevel.otel4s.{Attributes, TraceContext, Value}
import org.typelevel.otel4s.logs.Severity
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope

import scala.concurrent.duration.FiniteDuration

/** Represents a log record data within the logging system.
  *
  * `LogRecordData` serves as a core abstraction for log data used by exporters or other components that process logs.
  * Implementations of this trait are expected to encapsulate all necessary information about individual log entries
  * such as resource, instrumentation scope, and other contextual fields relevant for export purposes.
  *
  * This trait defines the base functionality for working with log data, particularly in systems adhering to
  * OpenTelemetry or similar structured logging standards.
  *
  * @see
  *   [[https://opentelemetry.io/docs/specs/otel/logs/data-model/]]
  */
sealed trait LogRecordData {

  /** Time when the event occurred measured by the origin clock, i.e. the time at the source. This field is optional, it
    * may be missing if the source timestamp is unknown.
    */
  def timestamp: Option[FiniteDuration]

  /** Time when the event was observed by the collection system.
    */
  def observedTimestamp: FiniteDuration

  def traceContext: Option[TraceContext]

  /** The severity level.
    */
  def severity: Option[Severity]

  /** Severity text (also known as log level). This is the original string representation of the severity as it is known
    * at the source.
    */
  def severityText: Option[String]

  /** A value containing the body of the log record. Can be, for example, a human-readable string message (including
    * multi-line) describing the event in a free form, or it can be a structured data composed of arrays and maps of
    * other values.
    */
  def body: Option[Value]

  /** Additional information about the specific event occurrence.
    */
  def attributes: Attributes

  /** The instrumentation scope associated with the log.
    */
  def instrumentationScope: InstrumentationScope

  /** The resource associated with the log.
    */
  def resource: TelemetryResource

  override final def hashCode(): Int =
    Hash[LogRecordData].hash(this)

  override final def equals(obj: Any): Boolean =
    obj match {
      case other: LogRecordData => Hash[LogRecordData].eqv(this, other)
      case _                    => false
    }

  override final def toString: String =
    Show[LogRecordData].show(this)

}

object LogRecordData {

  /** Creates [[LogRecordData]] with the given arguments.
    *
    * @param timestamp
    *   time when the event occurred measured by the origin clock
    *
    * @param observedTimestamp
    *   time when the event was observed by the collection system
    *
    * @param traceContext
    *   trace context associated with the log record
    *
    * @param severity
    *   severity level of the log record
    *
    * @param severityText
    *   textual representation of the severity level
    *
    * @param body
    *   a value containing the body of the log record
    *
    * @param attributes
    *   set of attributes associated with the log record
    *
    * @param instrumentationScope
    *   the instrumentation scope information
    *
    * @param resource
    *   the resource associated with the log record
    */
  def apply(
      timestamp: Option[FiniteDuration],
      observedTimestamp: FiniteDuration,
      traceContext: Option[TraceContext],
      severity: Option[Severity],
      severityText: Option[String],
      body: Option[Value],
      attributes: Attributes,
      instrumentationScope: InstrumentationScope,
      resource: TelemetryResource
  ): LogRecordData =
    Impl(
      timestamp = timestamp,
      observedTimestamp = observedTimestamp,
      traceContext = traceContext,
      severity = severity,
      severityText = severityText,
      body = body,
      attributes = attributes,
      instrumentationScope = instrumentationScope,
      resource = resource
    )

  implicit val logRecordDataHash: Hash[LogRecordData] =
    Hash.by { data =>
      (
        data.timestamp,
        data.observedTimestamp,
        // data.traceContext,
        data.severity,
        data.severityText,
        data.body,
        data.attributes,
        data.instrumentationScope,
        data.resource
      )
    }

  implicit val logRecordDataShow: Show[LogRecordData] =
    Show.show { data =>
      val traceContextStr = data.traceContext.fold("None") { tc =>
        s"Some(TraceContext{traceId=${tc.traceId}, spanId=${tc.spanId}, isSampled=${tc.isSampled}})"
      }

      s"LogRecordData{timestamp=${data.timestamp}, observedTimestamp=${data.observedTimestamp}, traceContext=$traceContextStr, severity=${data.severity}, severityText=${data.severityText}, body=${data.body}, attributes=${data.attributes}, instrumentationScope=${data.instrumentationScope}, resource=${data.resource}}"
    }

  private final case class Impl(
      timestamp: Option[FiniteDuration],
      observedTimestamp: FiniteDuration,
      traceContext: Option[TraceContext],
      severity: Option[Severity],
      severityText: Option[String],
      body: Option[Value],
      attributes: Attributes,
      instrumentationScope: InstrumentationScope,
      resource: TelemetryResource
  ) extends LogRecordData

}
