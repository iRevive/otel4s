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

package org.typelevel.otel4s.oteljava.testkit
package logs.data

import cats.Hash
import cats.Show
import io.opentelemetry.api.common.KeyValue
import io.opentelemetry.api.common.Value
import io.opentelemetry.api.common.ValueType
import io.opentelemetry.api.logs.{Severity => JSeverity}
import io.opentelemetry.sdk.logs.data.{LogRecordData => JLogRecordData}
import org.typelevel.otel4s.AnyValue
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.logs.Severity
import org.typelevel.otel4s.oteljava.AttributeConverters

import java.nio.ByteBuffer
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** A representation of the `io.opentelemetry.sdk.logs.data.LogRecordData`.
  */
sealed trait LogRecord {

  /** Time when the event occurred measured by the origin clock, i.e. the time at the source. This field is optional, it
    * may be missing if the source timestamp is unknown.
    */
  def timestamp: Option[FiniteDuration]

  /** Time when the event was observed by the collection system.
    */
  def observedTimestamp: FiniteDuration

  /** The tracing context associated with the log record.
    */
  // def traceContext: Option[TraceContext] // TODO

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
  def body: Option[AnyValue]

  /** Additional information about the specific event occurrence.
    */
  def attributes: Attributes

  /** The instrumentation scope associated with the log.
    */
  def scope: InstrumentationScope

  /** The resource associated with the log.
    */
  def resource: TelemetryResource

  override def hashCode(): Int =
    Hash[LogRecord].hash(this)

  override def toString: String =
    Show[LogRecord].show(this)

  override def equals(obj: Any): Boolean =
    obj match {
      case other: LogRecord =>
        Hash[LogRecord].eqv(this, other)
      case _ =>
        false
    }

}

object LogRecord {

  def apply(
      timestamp: Option[FiniteDuration],
      observedTimestamp: FiniteDuration,
      severity: Option[Severity],
      severityText: Option[String],
      body: Option[AnyValue],
      attributes: Attributes,
      scope: InstrumentationScope,
      resource: TelemetryResource
  ): LogRecord =
    Impl(timestamp, observedTimestamp, severity, severityText, body, attributes, scope, resource)

  def apply(logRecordData: JLogRecordData): LogRecord = {
    import AttributeConverters._

    val timestamp = Option.when(logRecordData.getTimestampEpochNanos > 0)(
      logRecordData.getTimestampEpochNanos.nanos
    )
    val observedTimestamp = logRecordData.getObservedTimestampEpochNanos.nanos
    val severity = toScalaSeverity(logRecordData.getSeverity)
    val severityText = Option(logRecordData.getSeverityText)
    val body = Option(logRecordData.getBodyValue).map(toAnyValue)
    val attributes = logRecordData.getAttributes.toScala
    val scope = InstrumentationScope(logRecordData.getInstrumentationScopeInfo)
    val resource = TelemetryResource(logRecordData.getResource)

    Impl(
      timestamp = timestamp,
      observedTimestamp = observedTimestamp,
      severity = severity,
      severityText = severityText,
      body = body,
      attributes = attributes,
      scope = scope,
      resource = resource
    )
  }

  private def toAnyValue(value: Value[_]): AnyValue =
    value.getType match {
      case ValueType.STRING =>
        AnyValue.string(value.getValue.asInstanceOf[String])

      case ValueType.BOOLEAN =>
        AnyValue.boolean(value.getValue.asInstanceOf[Boolean])

      case ValueType.LONG =>
        AnyValue.long(value.getValue.asInstanceOf[Long])

      case ValueType.DOUBLE =>
        AnyValue.double(value.getValue.asInstanceOf[Double])

      case ValueType.BYTES =>
        AnyValue.bytes(value.getValue.asInstanceOf[ByteBuffer].array())

      case ValueType.ARRAY =>
        AnyValue.list(
          value.getValue.asInstanceOf[java.util.List[Value[_]]].asScala.toSeq.map(toAnyValue)
        )

      case ValueType.KEY_VALUE_LIST =>
        AnyValue.map(
          value.getValue
            .asInstanceOf[java.util.List[KeyValue]]
            .asScala
            .toSeq
            .map(kv => (kv.getKey, toAnyValue(kv.getValue)))
            .toMap
        )
    }

  private def toScalaSeverity(severity: JSeverity): Option[Severity] =
    severity match {
      case JSeverity.UNDEFINED_SEVERITY_NUMBER => None
      case JSeverity.TRACE                     => Some(Severity.Trace.trace1)
      case JSeverity.TRACE2                    => Some(Severity.Trace.trace2)
      case JSeverity.TRACE3                    => Some(Severity.Trace.trace3)
      case JSeverity.TRACE4                    => Some(Severity.Trace.trace4)
      case JSeverity.DEBUG                     => Some(Severity.Debug.debug1)
      case JSeverity.DEBUG2                    => Some(Severity.Debug.debug2)
      case JSeverity.DEBUG3                    => Some(Severity.Debug.debug3)
      case JSeverity.DEBUG4                    => Some(Severity.Debug.debug4)
      case JSeverity.INFO                      => Some(Severity.Info.info1)
      case JSeverity.INFO2                     => Some(Severity.Info.info2)
      case JSeverity.INFO3                     => Some(Severity.Info.info3)
      case JSeverity.INFO4                     => Some(Severity.Info.info4)
      case JSeverity.WARN                      => Some(Severity.Warn.warn1)
      case JSeverity.WARN2                     => Some(Severity.Warn.warn1)
      case JSeverity.WARN3                     => Some(Severity.Warn.warn1)
      case JSeverity.WARN4                     => Some(Severity.Warn.warn1)
      case JSeverity.ERROR                     => Some(Severity.Error.error1)
      case JSeverity.ERROR2                    => Some(Severity.Error.error2)
      case JSeverity.ERROR3                    => Some(Severity.Error.error3)
      case JSeverity.ERROR4                    => Some(Severity.Error.error4)
      case JSeverity.FATAL                     => Some(Severity.Fatal.fatal1)
      case JSeverity.FATAL2                    => Some(Severity.Fatal.fatal2)
      case JSeverity.FATAL3                    => Some(Severity.Fatal.fatal3)
      case JSeverity.FATAL4                    => Some(Severity.Fatal.fatal4)
    }

  implicit val logRecordHash: Hash[LogRecord] =
    Hash.fromUniversalHashCode

  implicit val logRecordShow: Show[LogRecord] =
    Show.show(p =>
      s"LogRecord(${p.body}, ${p.attributes}, ${p.timestamp}, ${p.observedTimestamp}, ${p.severity}, ${p.severityText}, ${p.scope}, ${p.resource})"
    )

  final case class Impl(
      timestamp: Option[FiniteDuration],
      observedTimestamp: FiniteDuration,
      severity: Option[Severity],
      severityText: Option[String],
      body: Option[AnyValue],
      attributes: Attributes,
      scope: InstrumentationScope,
      resource: TelemetryResource
  ) extends LogRecord
}
