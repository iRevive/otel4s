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

package org.typelevel.otel4s.sdk.logs.exporter

import cats.Foldable
import cats.Monad
import cats.effect.std.Console
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import org.typelevel.otel4s.sdk.logs.data.LogRecordData

/** A log record exporter that logs every record using [[cats.effect.std.Console]].
  *
  * @note
  *   use this exporter for debugging purposes because it may affect the performance
  *
  * @tparam F
  *   the higher-kinded type of a polymorphic effect
  */
private final class ConsoleLogRecordExporter[F[_]: Monad: Console] extends LogRecordExporter[F] {

  val name: String = "ConsoleLogRecordExporter"

  def exportLogRecords[G[_]: Foldable](logs: G[LogRecordData]): F[Unit] = {
    for {
      _ <- Console[F].println(
        s"ConsoleLogRecordExporter: received a collection of [${logs.size}] log records for export."
      )
      _ <- logs.traverse_ { log =>
        Console[F].println(s"ConsoleLogRecordExporter: $log")
      }
    } yield ()
  }.whenA(logs.nonEmpty)

  def flush: F[Unit] = Monad[F].unit
}

object ConsoleLogRecordExporter {

  /** Creates a log record exporter that logs every record using [[cats.effect.std.Console]].
    *
    * @tparam F
    *   the higher-kinded type of a polymorphic effect
    */
  def apply[F[_]: Monad: Console]: LogRecordExporter[F] =
    new ConsoleLogRecordExporter[F]
}
