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

package org.typelevel.otel4s.sdk.metrics.autoconfigure

import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.std.Console
import cats.effect.std.Random
import cats.syntax.traverse._
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.autoconfigure.AutoConfigure
import org.typelevel.otel4s.sdk.autoconfigure.Config
import org.typelevel.otel4s.sdk.context.AskContext
import org.typelevel.otel4s.sdk.metrics.SdkMeterProvider
import org.typelevel.otel4s.sdk.metrics.autoconfigure.MeterProviderAutoConfigure.Customizer
import org.typelevel.otel4s.sdk.metrics.exporter.MetricExporter
import org.typelevel.otel4s.sdk.metrics.exporter.MetricReader
import org.typelevel.otel4s.sdk.metrics.exporter.PeriodicMetricReader

import scala.concurrent.duration._

private final class MeterProviderAutoConfigure[
    F[_]: Temporal: Random: Console: AskContext
](
    resource: TelemetryResource,
    customizer: Customizer[SdkMeterProvider.Builder[F]],
    exporterConfigurers: Set[AutoConfigure.Named[F, MetricExporter[F]]]
) extends AutoConfigure.WithHint[F, MeterProvider[F]](
      "MeterProvider",
      Set.empty
    ) {

  protected def fromConfig(config: Config): Resource[F, MeterProvider[F]] = {
    val exporterAutoConfigure =
      MetricExportersAutoConfigure[F](exporterConfigurers)

    for {
      exporters <- exporterAutoConfigure.configure(config)
      readers <- configureReaders(config, exporters)

      tracerProviderBuilder = {
        val builder = SdkMeterProvider
          .builder[F]
          .withResource(resource)

        readers.foldLeft(builder)(_.registerMetricReader(_))
      }

      tracerProvider <- Resource.eval(
        customizer(tracerProviderBuilder, config).build
      )
    } yield tracerProvider
  }

  private def configureReaders(
      config: Config,
      exporters: Map[String, MetricExporter[F]]
  ): Resource[F, List[MetricReader[F]]] = {
    val _ = config
    /*val loggingExporter = MetricExportersAutoConfigure.Const.LoggingExporter

    val logging = exporters.get(loggingExporter) match {
      case Some(logging) => List(SimpleSpanProcessor(logging))
      case None          => Nil
    }

    val others = exporters.removed(loggingExporter)
    if (others.nonEmpty) {
      val exporter = others.values.toList.combineAll
      BatchSpanProcessorAutoConfigure[F](exporter)
        .configure(config)
        .map(processor => logging :+ processor)
    } else {
      Resource.pure(logging)
    }*/
    exporters.values.toList.traverse { exporter =>
      PeriodicMetricReader.create(exporter, 1.minute)
    }
  }

}

object MeterProviderAutoConfigure {

  type Customizer[A] = (A, Config) => A

  /** Autoconfigures
    * [[org.typelevel.otel4s.metrics.MeterProvider MeterProvider]].
    *
    * @see
    *   [[MetricExportersAutoConfigure]]
    *
    * @param resource
    *   the resource to use
    *
    * @param meterProviderBuilderCustomizer
    *   the function to customize the builder
    *
    * @param exporterConfigurers
    *   the extra exporter configurers
    */
  def apply[F[_]: Temporal: Random: Console: AskContext](
      resource: TelemetryResource,
      meterProviderBuilderCustomizer: Customizer[SdkMeterProvider.Builder[F]],
      exporterConfigurers: Set[AutoConfigure.Named[F, MetricExporter[F]]]
  ): AutoConfigure[F, MeterProvider[F]] =
    new MeterProviderAutoConfigure[F](
      resource,
      meterProviderBuilderCustomizer,
      exporterConfigurers
    )

}
