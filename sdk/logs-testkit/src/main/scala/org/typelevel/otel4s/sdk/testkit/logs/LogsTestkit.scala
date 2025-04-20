/*
 * Copyright 2024 Typelevel
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

package org.typelevel.otel4s.sdk.testkit.logs

import cats.Parallel
import cats.effect.{Resource, Temporal}
import cats.effect.std.Console
import cats.mtl.Ask
import org.typelevel.otel4s.logs.LoggerProvider
import org.typelevel.otel4s.sdk.context.AskContext
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.logs.SdkLoggerProvider
import org.typelevel.otel4s.sdk.logs.data.LogRecordData
import org.typelevel.otel4s.sdk.logs.processor.SimpleLogRecordProcessor

trait LogsTestkit[F[_]] {

  /** The [[org.typelevel.otel4s.logs.LoggerProvider LoggerProvider]].
    */
  def loggerProvider: LoggerProvider[F]

  /** Collects and returns logs.
    *
    * @note
    *   logs are recollected on each invocation
    */
  def collectLogs: F[List[LogRecordData]]
}

object LogsTestkit {

  /** Creates [[LogsTestkit]] that keeps logs in-memory.
    *
    * @param customize
    *   the customization of the builder
    */
  def inMemory[F[_]: Temporal: Parallel: Console](
      customize: SdkLoggerProvider.Builder[F] => SdkLoggerProvider.Builder[F] = (b: SdkLoggerProvider.Builder[F]) => b
  ): Resource[F, LogsTestkit[F]] = {
    implicit val askContext: AskContext[F] = Ask.const(Context.root)
    create[F](customize)
  }

  private[sdk] def create[F[_]: Temporal: Parallel: Console: AskContext](
      customize: SdkLoggerProvider.Builder[F] => SdkLoggerProvider.Builder[F]
  ): Resource[F, LogsTestkit[F]] = {
    def createLoggerProvider(
        exporter: InMemoryLogRecordExporter[F]
    ): F[LoggerProvider[F]] = {
      val processor = SimpleLogRecordProcessor[F](exporter)
      val builder = SdkLoggerProvider.builder[F].addLogRecordProcessor(processor)
      customize(builder).build
    }

    for {
      exporter <- Resource.eval(InMemoryLogRecordExporter.create[F](None))
      loggerProvider <- Resource.eval(createLoggerProvider(exporter))
    } yield new Impl(loggerProvider, exporter)
  }

  private final class Impl[F[_]](
      val loggerProvider: LoggerProvider[F],
      exporter: InMemoryLogRecordExporter[F]
  ) extends LogsTestkit[F] {
    def collectLogs: F[List[LogRecordData]] =
      exporter.exportedLogs
  }

}
