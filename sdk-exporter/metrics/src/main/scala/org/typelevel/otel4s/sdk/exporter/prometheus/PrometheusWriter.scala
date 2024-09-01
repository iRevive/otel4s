package org.typelevel.otel4s.sdk.exporter.prometheus

import cats.Foldable
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import fs2.Stream
import fs2.text.utf8

trait PrometheusWriter[F[_]] {
  def contentType: String
  def write[G[_]: Foldable](metrics: G[MetricData]): Stream[F, Byte]
}

object PrometheusWriter {

  def text[F[_]]: PrometheusWriter[F] =
    new TextWriter[F]

  private final class TextWriter[F[_]] extends PrometheusWriter[F] {
    val contentType: String = "text/plain; version=0.0.4; charset=utf-8"

    def write[G[_]: Foldable](metrics: G[MetricData]): Stream[F, Byte] =
      Stream
        .foldable(metrics)
        .covary[F]
        .map(serialize)
        .through(utf8.encode)

    // todo
    private def serialize(metric: MetricData): String =
      metric.toString
  }

}
