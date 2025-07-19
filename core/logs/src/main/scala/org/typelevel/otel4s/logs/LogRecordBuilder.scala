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

package org.typelevel.otel4s.logs

import cats.Applicative
import cats.Monad
import org.typelevel.otel4s.AnyValue
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.KindTransformer
import org.typelevel.otel4s.meta.InstrumentMeta

import java.time.Instant
import scala.collection.immutable
import scala.concurrent.duration.FiniteDuration

/** @see
  *   [[https://opentelemetry.io/docs/specs/otel/logs/api/#emit-a-logrecord]]
  */
trait LogRecordBuilder[F[_], Ctx] {

  /** The instrument's metadata. Indicates whether instrumentation is enabled.
    */
  def meta: InstrumentMeta.Dynamic[F]

  def withTimestamp(timestamp: FiniteDuration): LogRecordBuilder[F, Ctx]

  def withTimestamp(timestamp: Instant): LogRecordBuilder[F, Ctx]

  def withObservedTimestamp(timestamp: FiniteDuration): LogRecordBuilder[F, Ctx]

  def withObservedTimestamp(timestamp: Instant): LogRecordBuilder[F, Ctx]

  def withContext(context: Ctx): LogRecordBuilder[F, Ctx]

  def withSeverity(severity: Severity): LogRecordBuilder[F, Ctx]

  def withSeverityText(severityText: String): LogRecordBuilder[F, Ctx]

  def withBody(body: AnyValue): LogRecordBuilder[F, Ctx]

  def addAttribute[A](attribute: Attribute[A]): LogRecordBuilder[F, Ctx]

  def addAttributes(attributes: Attribute[_]*): LogRecordBuilder[F, Ctx]

  def addAttributes(attributes: immutable.Iterable[Attribute[_]]): LogRecordBuilder[F, Ctx]

  def emit: F[Unit]

  def mapK[G[_]: Monad](implicit kt: KindTransformer[F, G]): LogRecordBuilder[G, Ctx] =
    new LogRecordBuilder.MappedK(this)
}

object LogRecordBuilder {

  def noop[F[_]: Applicative, Ctx]: LogRecordBuilder[F, Ctx] =
    new LogRecordBuilder[F, Ctx] {
      val meta: InstrumentMeta.Dynamic[F] = InstrumentMeta.Dynamic.disabled
      def withTimestamp(timestamp: FiniteDuration): LogRecordBuilder[F, Ctx] = this
      def withTimestamp(timestamp: Instant): LogRecordBuilder[F, Ctx] = this
      def withObservedTimestamp(timestamp: FiniteDuration): LogRecordBuilder[F, Ctx] = this
      def withObservedTimestamp(timestamp: Instant): LogRecordBuilder[F, Ctx] = this
      def withContext(context: Ctx): LogRecordBuilder[F, Ctx] = this
      def withSeverity(severity: Severity): LogRecordBuilder[F, Ctx] = this
      def withSeverityText(severityText: String): LogRecordBuilder[F, Ctx] = this
      def withBody(body: AnyValue): LogRecordBuilder[F, Ctx] = this
      def addAttribute[A](attribute: Attribute[A]): LogRecordBuilder[F, Ctx] = this
      def addAttributes(attributes: Attribute[_]*): LogRecordBuilder[F, Ctx] = this
      def addAttributes(attributes: immutable.Iterable[Attribute[_]]): LogRecordBuilder[F, Ctx] = this
      def emit: F[Unit] = Applicative[F].unit
    }

  private final class MappedK[F[_], G[_]: Monad, Ctx](
      builder: LogRecordBuilder[F, Ctx]
  )(implicit kt: KindTransformer[F, G])
      extends LogRecordBuilder[G, Ctx] {

    val meta: InstrumentMeta.Dynamic[G] = builder.meta.mapK[G]

    def withTimestamp(timestamp: FiniteDuration): LogRecordBuilder[G, Ctx] =
      builder.withTimestamp(timestamp).mapK

    def withTimestamp(timestamp: Instant): LogRecordBuilder[G, Ctx] =
      builder.withTimestamp(timestamp).mapK

    def withObservedTimestamp(timestamp: FiniteDuration): LogRecordBuilder[G, Ctx] =
      builder.withObservedTimestamp(timestamp).mapK

    def withObservedTimestamp(timestamp: Instant): LogRecordBuilder[G, Ctx] =
      builder.withObservedTimestamp(timestamp).mapK

    def withContext(context: Ctx): LogRecordBuilder[G, Ctx] =
      builder.withContext(context).mapK

    def withSeverity(severity: Severity): LogRecordBuilder[G, Ctx] =
      builder.withSeverity(severity).mapK

    def withSeverityText(severityText: String): LogRecordBuilder[G, Ctx] =
      builder.withSeverityText(severityText).mapK

    def withBody(body: AnyValue): LogRecordBuilder[G, Ctx] =
      builder.withBody(body).mapK

    def addAttribute[A](attribute: Attribute[A]): LogRecordBuilder[G, Ctx] =
      builder.addAttribute(attribute).mapK

    def addAttributes(attributes: Attribute[_]*): LogRecordBuilder[G, Ctx] =
      builder.addAttributes(attributes).mapK

    def addAttributes(attributes: immutable.Iterable[Attribute[_]]): LogRecordBuilder[G, Ctx] =
      builder.addAttributes(attributes).mapK

    def emit: G[Unit] =
      kt.liftK(builder.emit)
  }

}
