package org.typelevel.otel4s.sdk.logs.processor

import cats.ApplicativeThrow
import cats.effect.std.Console
import cats.syntax.applicativeError._
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.logs.data.LogRecordData
import org.typelevel.otel4s.sdk.logs.exporter.LogRecordExporter

/** An implementation of the [[LogRecordProcessor]] that passes [[LogRecordData]] directly to the configured exporter.
  *
  * @note
  *   this processor exports logs individually upon completion, resulting in a single log per export request.
  *
  * @see
  *   [[https://opentelemetry.io/docs/specs/otel/logs/sdk/#simple-processor]]
  *
  * @tparam F
  *   the higher-kinded type of polymorphic effect
  */
private final class SimpleLogRecordProcessor[F[_]: ApplicativeThrow: Console] private (
    exporter: LogRecordExporter[F]
) extends LogRecordProcessor[F] {

  val name: String =
    s"SimpleLogRecordProcessor{exporter=${exporter.name}}"

  def onEmit(context: Context, logRecord: LogRecordData): F[Unit] =
    exporter.exportLogs(List(logRecord)).handleErrorWith { e =>
      Console[F].errorln(
        s"SimpleLogRecordProcessor: the export has failed: ${e.getMessage}\n${e.getStackTrace.mkString("\n")}\n"
      )
    }

  def forceFlush: F[Unit] =
    ApplicativeThrow[F].unit
}

object SimpleLogRecordProcessor {

  /** Creates a [[SimpleLogRecordProcessor]] that passes log records to the given `exporter`.
    *
    * @param exporter
    *   the [[LogRecordExporter]] to use
    */
  def apply[F[_]: ApplicativeThrow: Console](exporter: LogRecordExporter[F]): LogRecordProcessor[F] =
    new SimpleLogRecordProcessor[F](exporter)

}
