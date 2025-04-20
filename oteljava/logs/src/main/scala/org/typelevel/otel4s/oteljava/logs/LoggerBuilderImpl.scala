package org.typelevel.otel4s
package oteljava
package logs

import cats.effect.Sync
import io.opentelemetry.api.logs.{LoggerProvider => JLoggerProvider}
import org.typelevel.otel4s.logs.{Logger, LoggerBuilder}
import org.typelevel.otel4s.oteljava.context.AskContext

private[oteljava] final case class LoggerBuilderImpl[F[_]: Sync: AskContext](
    jLoggerProvider: JLoggerProvider,
    name: String,
    version: Option[String] = None,
    schemaUrl: Option[String] = None
) extends LoggerBuilder[F] {

  def withVersion(version: String): LoggerBuilder[F] =
    copy(version = Option(version))

  def withSchemaUrl(schemaUrl: String): LoggerBuilder[F] =
    copy(schemaUrl = Option(schemaUrl))

  def get: F[Logger[F]] = Sync[F].delay {
    val b = jLoggerProvider.loggerBuilder(name)
    version.foreach(b.setInstrumentationVersion)
    schemaUrl.foreach(b.setSchemaUrl)
    new LoggerImpl(b.build())
  }
}
