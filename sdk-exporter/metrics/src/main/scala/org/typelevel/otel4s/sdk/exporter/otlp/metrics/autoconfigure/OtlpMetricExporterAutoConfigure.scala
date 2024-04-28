package org.typelevel.otel4s.sdk.exporter.otlp.metrics.autoconfigure

import cats.effect.{Async, Resource}
import cats.effect.std.Console
import fs2.compression.Compression
import fs2.io.net.Network
import org.http4s.Headers
import org.typelevel.otel4s.sdk.autoconfigure.{AutoConfigure, Config}
import org.typelevel.otel4s.sdk.metrics.exporter.{AggregationSelector, AggregationTemporalitySelector, CardinalityLimitSelector, MetricExporter}
import org.http4s.client.Client
import org.typelevel.otel4s.sdk.exporter.otlp.Protocol
import org.typelevel.otel4s.sdk.exporter.otlp.autoconfigure.{OtlpHttpClientAutoConfigure, ProtocolAutoConfigure}
import org.typelevel.otel4s.sdk.exporter.otlp.metrics.{MetricsProtoEncoder, OtlpHttpMetricExporter}
import org.typelevel.otel4s.sdk.metrics.data.MetricData

/** Autoconfigures OTLP
  * [[org.typelevel.otel4s.sdk.metrics.exporter.MetricExporter MetricExporter]].
  *
  * @see
  *   [[ProtocolAutoConfigure]] for OTLP protocol configuration
  *
  * @see
  *   [[OtlpHttpClientAutoConfigure]] for OTLP HTTP client configuration
  *
  * @see
  *   [[https://opentelemetry.io/docs/languages/sdk-configuration/otlp-exporter/#otel_exporter_otlp_protocol]]
  */
private final class OtlpMetricExporterAutoConfigure[
    F[_]: Async: Network: Compression: Console
](customClient: Option[Client[F]])
    extends AutoConfigure.WithHint[F, MetricExporter[F]](
      "OtlpMetricExporter",
      Set.empty
    )
    with AutoConfigure.Named[F, MetricExporter[F]] {

  def name: String = "otlp"

  protected def fromConfig(config: Config): Resource[F, MetricExporter[F]] =
    ProtocolAutoConfigure.metrics[F].configure(config).flatMap {
      case Protocol.Http(encoding) =>
        import MetricsProtoEncoder.exportMetricsRequest
        import MetricsProtoEncoder.jsonPrinter

        val defaults = OtlpHttpClientAutoConfigure.Defaults(
          OtlpHttpMetricExporter.Defaults.Endpoint,
          OtlpHttpMetricExporter.Defaults.Endpoint.path.toString,
          Headers.empty,
          OtlpHttpMetricExporter.Defaults.Timeout,
          encoding
        )

        OtlpHttpClientAutoConfigure
          .metrics[F, MetricData](defaults, customClient)
          .configure(config)
          .map { client =>
            new OtlpHttpMetricExporter[F](
              client,
              AggregationTemporalitySelector.alwaysCumulative,
              AggregationSelector.default,
              CardinalityLimitSelector.default
            )
          }
    }

}

object OtlpMetricExporterAutoConfigure {

  /** Autoconfigures OTLP
    * [[org.typelevel.otel4s.sdk.metrics.exporter.MetricExporter MetricExporter]].
    *
    * The configuration depends on the `otel.exporter.otlp.protocol` or
    * `otel.exporter.otlp.metrics.protocol`.
    *
    * The supported protocols: `http/json`, `http/protobuf`.
    *
    * @see
    *   `OtlpHttpClientAutoConfigure` for the configuration details of the OTLP
    *   HTTP client
    */
  def apply[
      F[_]: Async: Network: Compression: Console
  ]: AutoConfigure.Named[F, MetricExporter[F]] =
    new OtlpMetricExporterAutoConfigure[F](None)

  /** Autoconfigures OTLP
    * [[org.typelevel.otel4s.sdk.metrics.exporter.MetricExporter MetricExporter]]
    * using the given client.
    *
    * The configuration depends on the `otel.exporter.otlp.protocol` or
    * `otel.exporter.otlp.metrics.protocol`.
    *
    * The supported protocols: `http/json`, `http/protobuf`.
    *
    * @see
    *   `OtlpHttpClientAutoConfigure` for the configuration details of the OTLP
    *   HTTP client
    *
    * @note
    *   the 'timeout' and 'tlsContext' settings will be ignored. You must
    *   preconfigure the client manually.
    *
    * @example
    *   {{{
    * import java.net.{InetSocketAddress, ProxySelector}
    * import java.net.http.HttpClient
    * import org.http4s.jdkhttpclient.JdkHttpClient
    *
    * val jdkHttpClient = HttpClient
    *   .newBuilder()
    *   .proxy(ProxySelector.of(InetSocketAddress.createUnresolved("localhost", 3312)))
    *   .build()
    *
    * OpenTelemetrySdk.autoConfigured[IO](
    *   _.addMetricExporterConfigurer(
    *     OtlpMetricExporterAutoConfigure.customClient[IO](JdkHttpClient(jdkHttpClient))
    *   )
    * )
    *   }}}
    *
    * @param client
    *   the custom http4s client to use
    */
  def customClient[
      F[_]: Async: Network: Compression: Console
  ](client: Client[F]): AutoConfigure.Named[F, MetricExporter[F]] =
    new OtlpMetricExporterAutoConfigure[F](Some(client))

}
