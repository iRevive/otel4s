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
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.autoconfigure.AutoConfigure
import org.typelevel.otel4s.sdk.autoconfigure.Config
import org.typelevel.otel4s.sdk.autoconfigure.ConfigurationError
import org.typelevel.otel4s.sdk.context.AskContext
import org.typelevel.otel4s.sdk.metrics.SdkMeterProvider
import org.typelevel.otel4s.sdk.metrics.autoconfigure.MeterProviderAutoConfigure.Customizer
import org.typelevel.otel4s.sdk.metrics.exemplar.ExemplarFilter
import org.typelevel.otel4s.sdk.metrics.exemplar.TraceContextLookup
import org.typelevel.otel4s.sdk.metrics.exporter.MetricExporter

import java.util.Locale

private final class MeterProviderAutoConfigure[
    F[_]: Temporal: Random: Console: AskContext
](
    resource: TelemetryResource,
    traceContextLookup: TraceContextLookup,
    customizer: Customizer[SdkMeterProvider.Builder[F]],
    exporterConfigurers: Set[AutoConfigure.Named[F, MetricExporter[F]]]
) extends AutoConfigure.WithHint[F, MeterProvider[F]](
      "MeterProvider",
      MeterProviderAutoConfigure.ConfigKeys.All
    ) {

  import MeterProviderAutoConfigure.ConfigKeys
  import MeterProviderAutoConfigure.Defaults

  private val exemplarFilterConfigurer
      : Set[AutoConfigure.Named[F, ExemplarFilter]] = Set(
    AutoConfigure.Named.const("always_on", ExemplarFilter.alwaysOn),
    AutoConfigure.Named.const("always_off", ExemplarFilter.alwaysOff),
    AutoConfigure.Named.const(
      "trace_based",
      ExemplarFilter.traceBased(traceContextLookup)
    )
  )

  protected def fromConfig(config: Config): Resource[F, MeterProvider[F]] = {
    val exportersAutoConfigure =
      MetricExportersAutoConfigure[F](exporterConfigurers)

    def readersAutoConfigure(exporters: Set[MetricExporter[F]]) =
      MetricReadersAutoConfigure[F](exporters)

    for {
      filter <- exemplarFilter(config)
      exporters <- exportersAutoConfigure.configure(config)
      readers <- readersAutoConfigure(exporters.values.toSet).configure(config)

      meterProviderBuilder = {
        val builder = SdkMeterProvider
          .builder[F]
          .withResource(resource)
          .withExemplarFilter(filter)
          .withTraceContextLookup(traceContextLookup)

        readers.foldLeft(builder)(_.registerMetricReader(_))
      }

      provider <- Resource.eval(customizer(meterProviderBuilder, config).build)
    } yield provider
  }

  private def exemplarFilter(config: Config): Resource[F, ExemplarFilter] = {
    val value = config
      .getOrElse(ConfigKeys.ExemplarFilter, Defaults.ExemplarFilter)
      .map(_.toLowerCase(Locale.ROOT))

    value match {
      case Right(name) =>
        exemplarFilterConfigurer.find(_.name == name) match {
          case Some(configure) =>
            configure.configure(config)

          case None =>
            Resource.raiseError(
              ConfigurationError.unrecognized(
                ConfigKeys.ExemplarFilter.name,
                name,
                exemplarFilterConfigurer.map(_.name)
              ): Throwable
            )
        }

      case Left(error) =>
        Resource.raiseError(error: Throwable)
    }
  }
}

private[sdk] object MeterProviderAutoConfigure {

  private object ConfigKeys {
    val ExemplarFilter: Config.Key[String] =
      Config.Key("otel.metrics.exemplar.filter")

    val All: Set[Config.Key[_]] = Set(ExemplarFilter)
  }

  private object Defaults {
    val ExemplarFilter: String = "trace_based"
  }

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
      traceContextLookup: TraceContextLookup,
      meterProviderBuilderCustomizer: Customizer[SdkMeterProvider.Builder[F]],
      exporterConfigurers: Set[AutoConfigure.Named[F, MetricExporter[F]]]
  ): AutoConfigure[F, MeterProvider[F]] =
    new MeterProviderAutoConfigure[F](
      resource,
      traceContextLookup,
      meterProviderBuilderCustomizer,
      exporterConfigurers
    )

}
