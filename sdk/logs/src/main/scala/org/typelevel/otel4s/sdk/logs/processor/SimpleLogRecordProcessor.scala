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
