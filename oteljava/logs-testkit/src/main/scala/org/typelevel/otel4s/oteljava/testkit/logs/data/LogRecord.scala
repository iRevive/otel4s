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
import cats.syntax.show._
import io.opentelemetry.sdk.logs.data.{LogRecordData => JLogRecordData}
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.logs.Severity

import java.time.Instant

/** A representation of the `io.opentelemetry.sdk.logs.data.LogRecordData`.
  */
sealed trait LogRecord {

  def body: String

  def attributes: Attributes

  def timestamp: Instant

  def observedTimestamp: Instant

  def severity: Severity

  def severityText: String

  def scope: InstrumentationScope

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
      body: String,
      attributes: Attributes,
      timestamp: Instant,
      observedTimestamp: Instant,
      severity: Severity,
      severityText: String,
      scope: InstrumentationScope,
      resource: TelemetryResource
  ): LogRecord =
    Impl(body, attributes, timestamp, observedTimestamp, severity, severityText, scope, resource)

  def apply(logRecordData: JLogRecordData): LogRecord = {
    // Simple implementation that doesn't rely on complex conversions
    val body = Option(logRecordData.getBody).map(_.toString).getOrElse("")
    val attributes = Attributes.empty // Simplified for now
    val timestamp = Instant.now() // Simplified for now
    val observedTimestamp = Instant.now() // Simplified for now
    val severity = Severity.Info.info1 // Default to Info.info1
    val severityText = Option(logRecordData.getSeverityText).getOrElse("")
    val scope = InstrumentationScope(logRecordData.getInstrumentationScopeInfo)
    val resource = TelemetryResource(logRecordData.getResource)

    Impl(body, attributes, timestamp, observedTimestamp, severity, severityText, scope, resource)
  }

  implicit val logRecordHash: Hash[LogRecord] =
    Hash.fromUniversalHashCode

  implicit val logRecordShow: Show[LogRecord] =
    Show.show(p =>
      s"LogRecord(${p.body}, ${p.attributes}, ${p.timestamp}, ${p.observedTimestamp}, ${p.severity}, ${p.severityText}, ${p.scope}, ${p.resource})"
    )

  final case class Impl(
      body: String,
      attributes: Attributes,
      timestamp: Instant,
      observedTimestamp: Instant,
      severity: Severity,
      severityText: String,
      scope: InstrumentationScope,
      resource: TelemetryResource
  ) extends LogRecord
}
