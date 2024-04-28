package org.typelevel.otel4s.sdk.exporter.otlp.autoconfigure

import cats.effect.Async
import cats.effect.std.Console
import fs2.compression.Compression
import fs2.io.net.Network
import org.http4s.client.Client
import org.typelevel.otel4s.sdk.autoconfigure.ExporterAutoConfigure
import org.typelevel.otel4s.sdk.exporter.otlp.trace.autoconfigure.OtlpSpanExporterAutoConfigure
import org.typelevel.otel4s.sdk.exporter.otlp.metrics.autoconfigure.OtlpMetricExporterAutoConfigure

object OtlpExporterAutoConfigure {

  def apply[
      F[_]: Async: Network: Compression: Console
  ]: ExporterAutoConfigure[F] =
    ExporterAutoConfigure(
      OtlpMetricExporterAutoConfigure[F],
      OtlpSpanExporterAutoConfigure[F]
    )

  def customClient[
      F[_]: Async: Network: Compression: Console
  ](client: Client[F]): ExporterAutoConfigure[F] =
    ExporterAutoConfigure(
      OtlpMetricExporterAutoConfigure.customClient[F](client),
      OtlpSpanExporterAutoConfigure.customClient[F](client)
    )

}
