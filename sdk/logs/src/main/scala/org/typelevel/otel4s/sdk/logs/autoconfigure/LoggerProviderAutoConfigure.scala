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

package org.typelevel.otel4s.sdk.logs.autoconfigure

import cats.Parallel
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.std.Console
import cats.syntax.all._
import org.typelevel.otel4s.logs.LoggerProvider
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.autoconfigure.AutoConfigure
import org.typelevel.otel4s.sdk.autoconfigure.Config
import org.typelevel.otel4s.sdk.context.AskContext
import org.typelevel.otel4s.sdk.logs.SdkLoggerProvider
import org.typelevel.otel4s.sdk.logs.autoconfigure.LoggerProviderAutoConfigure.Customizer
import org.typelevel.otel4s.sdk.logs.exporter.LogRecordExporter
import org.typelevel.otel4s.sdk.logs.processor.{LogRecordProcessor, SimpleLogRecordProcessor}

/** Autoconfigures [[org.typelevel.otel4s.logs.LoggerProvider LoggerProvider]].
  *
  * @param resource
  *   the resource to use
  *
  * @param customizer
  *   the function to customize the builder
  *
  * @param exporterConfigurers
  *   the extra exporter configurers
  */
private final class LoggerProviderAutoConfigure[F[_]: Temporal: Parallel: Console: AskContext](
    resource: TelemetryResource,
    customizer: Customizer[SdkLoggerProvider.Builder[F]], // todo: trace context lookup
    exporterConfigurers: Set[AutoConfigure.Named[F, LogRecordExporter[F]]]
) extends AutoConfigure.WithHint[F, LoggerProvider[F]](
      "LoggerProvider",
      Set.empty
    ) {

  protected def fromConfig(config: Config): Resource[F, LoggerProvider[F]] =
    for {
      exporters <- LogRecordExportersAutoConfigure[F](exporterConfigurers).configure(config)
      processors <- configureProcessors(config, exporters)
      logLimits <- LogLimitsAutoConfigure[F].configure(config)
      loggerProviderBuilder = {
        val builder = SdkLoggerProvider
          .builder[F]
          .withResource(resource)
          .withLogLimits(logLimits)

        processors.foldLeft(builder)(_.addLogRecordProcessor(_))
      }

      loggerProvider <- Resource.eval(
        customizer(loggerProviderBuilder, config).build
      )
    } yield loggerProvider

  private def configureProcessors(
      config: Config,
      exporters: Map[String, LogRecordExporter[F]]
  ): Resource[F, List[LogRecordProcessor[F]]] = {
    val consoleExporter = LogRecordExportersAutoConfigure.Const.ConsoleExporter

    val console = exporters.get(consoleExporter) match {
      case Some(console) => List(SimpleLogRecordProcessor(console))
      case None          => Nil
    }

    val others = exporters.removed(consoleExporter)
    if (others.nonEmpty) {
      val exporter = others.values.toList.combineAll
      BatchLogRecordProcessorAutoConfigure[F](exporter)
        .configure(config)
        .map(processor => console :+ processor)
    } else {
      Resource.pure(console)
    }
  }
}

private[sdk] object LoggerProviderAutoConfigure {

  type Customizer[A] = (A, Config) => A

  /** Autoconfigures [[org.typelevel.otel4s.logs.LoggerProvider LoggerProvider]].
    *
    * @param resource
    *   the resource to use
    *
    * @param loggerProviderBuilderCustomizer
    *   the function to customize the builder
    *
    * @param exporterConfigurers
    *   the extra exporter configurers
    */
  def apply[F[_]: Temporal: Parallel: Console: AskContext](
      resource: TelemetryResource,
      loggerProviderBuilderCustomizer: Customizer[SdkLoggerProvider.Builder[F]],
      exporterConfigurers: Set[AutoConfigure.Named[F, LogRecordExporter[F]]]
  ): AutoConfigure[F, LoggerProvider[F]] =
    new LoggerProviderAutoConfigure[F](
      resource,
      loggerProviderBuilderCustomizer,
      exporterConfigurers
    )
}
