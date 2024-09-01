package org.typelevel.otel4s.sdk.exporter.prometheus

import cats.Monad
import cats.syntax.functor._
import org.http4s.{HttpRoutes, Response}
import org.typelevel.otel4s.sdk.metrics.exporter.MetricExporter

object PrometheusHttpRoutes {

  def routes[F[_]: Monad](exporter: MetricExporter.Pull[F]): HttpRoutes[F] = {
    val writer = PrometheusWriter.text[F]

    HttpRoutes.of {
      case req => // check routes, content type, etc
        for {
          metrics <- exporter.metricReader.collectAllMetrics
          // set content type, gzip if needed, etc
        } yield Response().withEntity(writer.write(metrics))
    }
  }

}
