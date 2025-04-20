package org.typelevel.otel4s
package oteljava
package logs

import cats.effect.Sync
import org.typelevel.otel4s.logs.{LogRecordBuilder, Logger}
import io.opentelemetry.api.logs.{Logger => JLogger}
import org.typelevel.otel4s.oteljava.context.AskContext

private[oteljava] final class LoggerImpl[F[_]: Sync: AskContext](jLogger: JLogger) extends Logger[F] {
  def logRecordBuilder: LogRecordBuilder[F] =
    new LogRecordBuilderImpl[F](jLogger.logRecordBuilder())
}
