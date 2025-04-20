package org.typelevel.otel4s.logs

import java.time.Instant

import cats.Applicative
import org.typelevel.otel4s.{Attribute, KindTransformer, Value}

import scala.collection.immutable
import scala.concurrent.duration.FiniteDuration

/** @see
  *   [[https://opentelemetry.io/docs/specs/otel/logs/api/#emit-a-logrecord]]
  */
trait LogRecordBuilder[F[_]] {

  def withTimestamp(timestamp: FiniteDuration): LogRecordBuilder[F]

  def withTimestamp(timestamp: Instant): LogRecordBuilder[F]

  def withObservedTimestamp(timestamp: FiniteDuration): LogRecordBuilder[F]

  def withObservedTimestamp(timestamp: Instant): LogRecordBuilder[F]

  /*def withTraceContext(context: TraceContext): LogRecordBuilder[F]*/

  def withSeverity(severity: Severity): LogRecordBuilder[F]

  def withSeverityText(severityText: String): LogRecordBuilder[F]

  def withBody(body: Value): LogRecordBuilder[F]

  def addAttribute[A](attribute: Attribute[A]): LogRecordBuilder[F]

  def addAttributes(attributes: Attribute[_]*): LogRecordBuilder[F]

  def addAttributes(attributes: immutable.Iterable[Attribute[_]]): LogRecordBuilder[F]

  def emit: F[Unit]

  def mapK[G[_]](implicit kt: KindTransformer[F, G]): LogRecordBuilder[G] =
    new LogRecordBuilder.MappedK(this)
}

object LogRecordBuilder {

  def noop[F[_]: Applicative]: LogRecordBuilder[F] =
    new LogRecordBuilder[F] {
      def withTimestamp(timestamp: FiniteDuration): LogRecordBuilder[F] = this

      def withTimestamp(timestamp: Instant): LogRecordBuilder[F] = this

      def withObservedTimestamp(timestamp: FiniteDuration): LogRecordBuilder[F] = this

      def withObservedTimestamp(timestamp: Instant): LogRecordBuilder[F] = this

      /*def withTraceContext(context: TraceContext): LogRecordBuilder[F] = this*/

      def withSeverity(severity: Severity): LogRecordBuilder[F] = this

      def withSeverityText(severityText: String): LogRecordBuilder[F] = this

      def withBody(body: Value): LogRecordBuilder[F] = this

      def addAttribute[A](attribute: Attribute[A]): LogRecordBuilder[F] = this

      def addAttributes(attributes: Attribute[_]*): LogRecordBuilder[F] = this

      def addAttributes(attributes: immutable.Iterable[Attribute[_]]): LogRecordBuilder[F] = this
      def emit: F[Unit] = Applicative[F].unit
    }

  private final class MappedK[F[_], G[_]](
      builder: LogRecordBuilder[F]
  )(implicit kt: KindTransformer[F, G])
      extends LogRecordBuilder[G] {

    def withTimestamp(timestamp: FiniteDuration): LogRecordBuilder[G] = this

    def withTimestamp(timestamp: Instant): LogRecordBuilder[G] = this

    def withObservedTimestamp(timestamp: FiniteDuration): LogRecordBuilder[G] = this

    def withObservedTimestamp(timestamp: Instant): LogRecordBuilder[G] = this

    /*def withTraceContext(context: TraceContext): LogRecordBuilder[G] = this*/

    def withSeverity(severity: Severity): LogRecordBuilder[G] = this

    def withSeverityText(severityText: String): LogRecordBuilder[G] = this

    def withBody(body: Value): LogRecordBuilder[G] = this

    def addAttribute[A](attribute: Attribute[A]): LogRecordBuilder[G] = this

    def addAttributes(attributes: Attribute[_]*): LogRecordBuilder[G] = this

    def addAttributes(attributes: immutable.Iterable[Attribute[_]]): LogRecordBuilder[G] = this

    def emit: G[Unit] =
      kt.liftK(builder.emit)
  }

}
