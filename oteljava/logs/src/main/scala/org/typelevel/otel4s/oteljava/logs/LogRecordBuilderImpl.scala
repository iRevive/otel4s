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

package org.typelevel.otel4s
package oteljava
package logs

import cats.effect.Sync
import cats.mtl.Ask
import cats.syntax.flatMap._
import io.opentelemetry.api.common.{AttributeKey => JAttributeKey}
import io.opentelemetry.api.common.{Value => JValue}
import io.opentelemetry.api.logs.{LogRecordBuilder => JLogRecordBuilder}
import io.opentelemetry.api.logs.{Severity => JSeverity}
import org.typelevel.otel4s.logs.LogRecordBuilder
import org.typelevel.otel4s.logs.Severity
import org.typelevel.otel4s.meta.InstrumentMeta
import org.typelevel.otel4s.oteljava.AttributeConverters._
import org.typelevel.otel4s.oteljava.context.AskContext
import org.typelevel.otel4s.oteljava.context.Context

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.collection.immutable
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._

private[oteljava] final case class LogRecordBuilderImpl[F[_]: Sync: AskContext](
    meta: InstrumentMeta.Dynamic[F],
    jBuilder: JLogRecordBuilder
) extends LogRecordBuilder[F, Context] {

  def withTimestamp(timestamp: FiniteDuration): LogRecordBuilder[F, Context] =
    copy(jBuilder = jBuilder.setTimestamp(timestamp.toNanos, TimeUnit.NANOSECONDS))

  def withTimestamp(timestamp: Instant): LogRecordBuilder[F, Context] =
    copy(jBuilder = jBuilder.setTimestamp(timestamp))

  def withObservedTimestamp(timestamp: FiniteDuration): LogRecordBuilder[F, Context] =
    copy(jBuilder = jBuilder.setObservedTimestamp(timestamp.toNanos, TimeUnit.NANOSECONDS))

  def withObservedTimestamp(timestamp: Instant): LogRecordBuilder[F, Context] =
    copy(jBuilder = jBuilder.setObservedTimestamp(timestamp))

  def withContext(context: Context): LogRecordBuilder[F, Context] =
    copy(jBuilder = jBuilder.setContext(context.underlying))

  def withSeverity(severity: Severity): LogRecordBuilder[F, Context] =
    copy(jBuilder = jBuilder.setSeverity(toJSeverity(severity)))

  def withSeverityText(severityText: String): LogRecordBuilder[F, Context] =
    copy(jBuilder = jBuilder.setSeverityText(severityText))

  def withBody(value: AnyValue): LogRecordBuilder[F, Context] =
    copy(jBuilder = jBuilder.setBody(toJValue(value)))

  def addAttribute[A](attribute: Attribute[A]): LogRecordBuilder[F, Context] =
    copy(jBuilder = jBuilder.setAttribute(attribute.key.toJava.asInstanceOf[JAttributeKey[Any]], attribute.value))

  def addAttributes(attributes: Attribute[_]*): LogRecordBuilder[F, Context] =
    copy(jBuilder = jBuilder.setAllAttributes(attributes.toJavaAttributes))

  def addAttributes(attributes: immutable.Iterable[Attribute[_]]): LogRecordBuilder[F, Context] =
    copy(jBuilder = jBuilder.setAllAttributes(attributes.toJavaAttributes))

  def emit: F[Unit] =
    Ask[F, Context].ask.flatMap { ctx =>
      Sync[F].delay {
        // make the current context active
        val scope = ctx.underlying.makeCurrent()
        try {
          jBuilder.emit()
        } finally {
          scope.close() // release the context
        }
      }
    }

  private def toJSeverity(severity: Severity): JSeverity =
    severity match {
      case Severity.Trace.Trace1 => JSeverity.TRACE
      case Severity.Trace.Trace2 => JSeverity.TRACE2
      case Severity.Trace.Trace3 => JSeverity.TRACE3
      case Severity.Trace.Trace4 => JSeverity.TRACE4

      case Severity.Debug.Debug1 => JSeverity.DEBUG
      case Severity.Debug.Debug2 => JSeverity.DEBUG2
      case Severity.Debug.Debug3 => JSeverity.DEBUG3
      case Severity.Debug.Debug4 => JSeverity.DEBUG4

      case Severity.Info.Info1 => JSeverity.INFO
      case Severity.Info.Info2 => JSeverity.INFO2
      case Severity.Info.Info3 => JSeverity.INFO3
      case Severity.Info.Info4 => JSeverity.INFO4

      case Severity.Warn.Warn1 => JSeverity.WARN
      case Severity.Warn.Warn2 => JSeverity.WARN2
      case Severity.Warn.Warn3 => JSeverity.WARN3
      case Severity.Warn.Warn4 => JSeverity.WARN4

      case Severity.Error.Error1 => JSeverity.ERROR
      case Severity.Error.Error2 => JSeverity.ERROR2
      case Severity.Error.Error3 => JSeverity.ERROR3
      case Severity.Error.Error4 => JSeverity.ERROR4

      case Severity.Fatal.Fatal1 => JSeverity.FATAL
      case Severity.Fatal.Fatal2 => JSeverity.FATAL2
      case Severity.Fatal.Fatal3 => JSeverity.FATAL3
      case Severity.Fatal.Fatal4 => JSeverity.FATAL4
    }

  private def toJValue(value: AnyValue): JValue[_] =
    value match {
      case AnyValue.StringValue(value)    => JValue.of(value)
      case AnyValue.BooleanValue(value)   => JValue.of(value)
      case AnyValue.LongValue(value)      => JValue.of(value)
      case AnyValue.DoubleValue(value)    => JValue.of(value)
      case AnyValue.ByteArrayValue(value) => JValue.of(value)
      case AnyValue.ArrayValue(values)    => JValue.of(values.map(toJValue).toList.asJava)
      case AnyValue.MapValue(values)      => JValue.of(values.view.mapValues(toJValue).toMap.asJava)
    }
}
