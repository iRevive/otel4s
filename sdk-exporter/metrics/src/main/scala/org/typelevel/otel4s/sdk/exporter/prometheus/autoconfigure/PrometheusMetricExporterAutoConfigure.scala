package org.typelevel.otel4s.sdk.exporter.prometheus
package autoconfigure

import cats.effect.{Async, Resource}
import cats.effect.std.Console
import com.comcast.ip4s._
import fs2.compression.Compression
import fs2.io.net.Network
import org.typelevel.otel4s.sdk.autoconfigure.{
  AutoConfigure,
  Config,
  ConfigurationError
}
import org.typelevel.otel4s.sdk.metrics.exporter.MetricExporter

private final class PrometheusMetricExporterAutoConfigure[
    F[_]: Async: Network: Compression: Console
] extends AutoConfigure.WithHint[F, MetricExporter[F]](
      "PrometheusMetricExporter",
      PrometheusMetricExporterAutoConfigure.ConfigKeys.All
    )
    with AutoConfigure.Named[F, MetricExporter[F]] {

  import PrometheusMetricExporterAutoConfigure.ConfigKeys
  import PrometheusMetricExporter.Defaults

  def name: String = "prometheus"

  protected def fromConfig(config: Config): Resource[F, MetricExporter[F]] =
    for {
      host <- Resource.eval(
        Async[F].fromEither(config.getOrElse(ConfigKeys.Host, Defaults.Host))
      )

      port <- Resource.eval(
        Async[F].fromEither(config.getOrElse(ConfigKeys.Port, Defaults.Port))
      )

      exporter <- PrometheusMetricExporter
        .serverBuilder[F]
        .withHost(host)
        .withPort(port)
        .build
    } yield exporter

  private implicit val hostReader: Config.Reader[Host] =
    Config.Reader.decodeWithHint("Host") { s =>
      Host.fromString(s).toRight(ConfigurationError("Cannot parse host"))
    }

  private implicit val portReader: Config.Reader[Port] =
    Config.Reader.decodeWithHint("Port") { s =>
      Port.fromString(s).toRight(ConfigurationError("Cannot parse port"))
    }

}

object PrometheusMetricExporterAutoConfigure {

  private object ConfigKeys {
    val Host: Config.Key[Host] =
      Config.Key("otel.exporter.prometheus.host")

    val Port: Config.Key[Port] =
      Config.Key("otel.exporter.prometheus.port")

    val All: Set[Config.Key[_]] = Set(Host, Port)
  }

  def apply[
      F[_]: Async: Network: Compression: Console
  ]: AutoConfigure.Named[F, MetricExporter[F]] =
    new PrometheusMetricExporterAutoConfigure[F]

}
