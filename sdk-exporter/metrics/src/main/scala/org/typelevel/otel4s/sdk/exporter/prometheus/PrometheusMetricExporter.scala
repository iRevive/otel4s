package org.typelevel.otel4s.sdk.exporter.prometheus

import cats.data.NonEmptyVector
import cats.effect.{Concurrent, MonadCancelThrow, Ref, Resource}
import cats.effect.kernel.Async
import cats.effect.std.Console
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.comcast.ip4s._
import fs2.io.net.Network
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.exporter._

private final class PrometheusMetricExporter[
    F[_]: MonadCancelThrow: Console
] private[prometheus] (
    metricProducers: Ref[F, Vector[MetricProducer[F]]],
    val defaultAggregationSelector: AggregationSelector,
    val defaultCardinalityLimitSelector: CardinalityLimitSelector
) extends MetricExporter.Pull[F] { self =>

  def name: String = "PrometheusMetricExporter"

  val aggregationTemporalitySelector: AggregationTemporalitySelector =
    AggregationTemporalitySelector.alwaysCumulative

  val metricReader: MetricReader[F] =
    new MetricReader[F] {
      def aggregationTemporalitySelector: AggregationTemporalitySelector =
        self.aggregationTemporalitySelector

      def defaultAggregationSelector: AggregationSelector =
        self.defaultAggregationSelector

      def defaultCardinalityLimitSelector: CardinalityLimitSelector =
        self.defaultCardinalityLimitSelector

      def register(producers: NonEmptyVector[MetricProducer[F]]): F[Unit] = {
        def warn =
          Console[F].errorln(
            "MetricProducers are already registered at this PrometheusMetricReader"
          )

        metricProducers.flatModify { current =>
          if (current.isEmpty) (producers.toVector, MonadCancelThrow[F].unit)
          else (current, warn)
        }
      }

      def collectAllMetrics: F[Vector[MetricData]] =
        metricProducers.get.flatMap {
          case producers if producers.nonEmpty =>
            producers.flatTraverse(_.produce)

          case _ =>
            Console[F]
              .errorln(
                "The PrometheusMetricReader is running, but producers aren't configured yet. Nothing to export"
              )
              .as(Vector.empty)
        }

      def forceFlush: F[Unit] =
        MonadCancelThrow[F].unit

      override def toString: String =
        "PrometheusMetricReader"
    }

}

object PrometheusMetricExporter {

  private[prometheus] object Defaults {
    val Host: Host = host"localhost"
    val Port: Port = port"9464"
  }

  sealed trait Builder[F[_]] {

    def withDefaultAggregationSelector(
        selector: AggregationSelector
    ): Builder[F]

    def withDefaultCardinalityLimitSelector(
        selector: CardinalityLimitSelector
    ): Builder[F]

    // all other options from https://opentelemetry.io/docs/specs/otel/metrics/sdk_exporters/prometheus/#configuration
    // except 'host' and 'port'

    def build: F[MetricExporter.Pull[F]]
  }

  sealed trait HttpServerBuilder[F[_]] {
    def withHost(host: Host): HttpServerBuilder[F]

    def withPort(port: Port): HttpServerBuilder[F]

    def withDefaultAggregationSelector(
        selector: AggregationSelector
    ): HttpServerBuilder[F]

    def withDefaultCardinalityLimitSelector(
        selector: CardinalityLimitSelector
    ): HttpServerBuilder[F]

    // all other options from https://opentelemetry.io/docs/specs/otel/metrics/sdk_exporters/prometheus/#configuration

    def build: Resource[F, MetricExporter.Pull[F]]
  }

  def serverBuilder[F[_]: Async: Network: Console]: HttpServerBuilder[F] =
    new ServerBuilder[F](
      host = Defaults.Host,
      port = Defaults.Port,
      defaultAggregationSelector = AggregationSelector.default,
      defaultCardinalityLimitSelector = CardinalityLimitSelector.default
    )

  def builder[F[_]: Concurrent: Console]: Builder[F] = ???

  private final case class ServerBuilder[F[_]: Async: Network: Console](
      host: Host,
      port: Port,
      defaultAggregationSelector: AggregationSelector,
      defaultCardinalityLimitSelector: CardinalityLimitSelector
  ) extends HttpServerBuilder[F] {
    def withHost(host: Host): HttpServerBuilder[F] =
      copy(host = host)

    def withPort(port: Port): HttpServerBuilder[F] =
      copy(port = port)

    def withDefaultAggregationSelector(
        selector: AggregationSelector
    ): HttpServerBuilder[F] = ???

    def withDefaultCardinalityLimitSelector(
        selector: CardinalityLimitSelector
    ): HttpServerBuilder[F] = ???

    def build: Resource[F, MetricExporter.Pull[F]] =
      Resource.eval(Ref.empty[F, Vector[MetricProducer[F]]]).flatMap { ref =>
        val exporter = new PrometheusMetricExporter[F](
          ref,
          defaultAggregationSelector,
          defaultCardinalityLimitSelector
        )

        val routes = PrometheusHttpRoutes.routes[F](exporter)

        org.http4s.ember.server.EmberServerBuilder
          .default[F]
          .withHost(host)
          .withPort(port)
          .withHttpApp(routes.orNotFound)
          .build
          .evalTap { server =>
            Console[F].println(s"PrometheusMetricsExporter: launched Prometheus server at ${server.address}")
          }
          .as(exporter)
      }

  }

}
