package org.typelevel.otel4s.sdk.autoconfigure

import org.typelevel.otel4s.sdk.metrics.exporter.MetricExporter
import org.typelevel.otel4s.sdk.trace.exporter.SpanExporter

sealed trait ExporterAutoConfigure[F[_]] {
  def metricExporterAutoConfigure: AutoConfigure.Named[F, MetricExporter[F]]
  def spanExporterAutoConfigure: AutoConfigure.Named[F, SpanExporter[F]]
}

object ExporterAutoConfigure {

  def apply[F[_]](
      metricExporterAutoConfigure: AutoConfigure.Named[F, MetricExporter[F]],
      spanExporterAutoConfigure: AutoConfigure.Named[F, SpanExporter[F]]
  ): ExporterAutoConfigure[F] =
    Impl(metricExporterAutoConfigure, spanExporterAutoConfigure)

  private final case class Impl[F[_]](
      metricExporterAutoConfigure: AutoConfigure.Named[F, MetricExporter[F]],
      spanExporterAutoConfigure: AutoConfigure.Named[F, SpanExporter[F]]
  ) extends ExporterAutoConfigure[F]

}
