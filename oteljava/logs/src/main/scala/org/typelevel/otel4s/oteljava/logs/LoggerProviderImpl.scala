package org.typelevel.otel4s
package oteljava
package logs

import cats.effect.Sync
import io.opentelemetry.api.logs.{LoggerProvider => JLoggerProvider}
import org.typelevel.otel4s.logs.{LoggerBuilder, LoggerProvider}
import org.typelevel.otel4s.oteljava.context.AskContext

private[oteljava] final class LoggerProviderImpl[F[_]: Sync: AskContext](
    jLoggerProvider: JLoggerProvider
) extends LoggerProvider[F] {

  def logger(name: String): LoggerBuilder[F] =
    LoggerBuilderImpl(jLoggerProvider, name)

}
