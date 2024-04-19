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

package org.typelevel.otel4s.sdk.metrics
package exporter

import cats.data.NonEmptyVector
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.std.Console
import cats.effect.syntax.spawn._
import cats.effect.syntax.temporal._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.typelevel.otel4s.sdk.metrics.data.MetricData

import scala.concurrent.duration.FiniteDuration

private class PeriodicMetricReader[F[_]: Temporal: Console] private (
    metricProducers: Ref[F, Vector[MetricProducer[F]]],
    exporter: MetricExporter[F]
) extends MetricReader[F] {

  def defaultAggregationSelector: AggregationSelector =
    exporter.defaultAggregationSelector

  def aggregationTemporalitySelector: AggregationTemporalitySelector =
    exporter.aggregationTemporalitySelector

  def defaultCardinalityLimitSelector: CardinalityLimitSelector =
    exporter.defaultCardinalityLimitSelector

  def register(producers: NonEmptyVector[MetricProducer[F]]): F[Unit] = {
    def warn =
      Console[F].error(
        "Metrics are already registered at this periodic metric reader"
      )

    metricProducers.flatModify { current =>
      if (current.isEmpty) (producers.toVector, Temporal[F].unit)
      else (current, warn)
    }
  }

  def collectAllMetrics: F[Vector[MetricData]] =
    metricProducers.get.flatMap {
      case producers if producers.nonEmpty =>
        producers.flatTraverse(_.produce)

      case _ =>
        Console[F]
          .error(
            "The PeriodicMetricReader is running, but producers aren't configured yet. Nothing to export"
          )
          .as(Vector.empty)
    }

  private def exportMetrics: F[Unit] = {
    val doExport =
      for {
        metrics <- collectAllMetrics
        _ <- exporter.exportMetrics(metrics).whenA(metrics.nonEmpty)
      } yield ()

    doExport.handleErrorWith { e =>
      Console[F].error(
        s"PeriodicMetricExporter: the export has failed: ${e.getMessage}\n${e.getStackTrace.mkString("\n")}\n"
      )
    }
  }

  def forceFlush: F[Unit] =
    for {
      _ <- exportMetrics
      _ <- exporter.flush
    } yield ()
}

private object PeriodicMetricReader {

  def create[F[_]: Temporal: Console](
      exporter: MetricExporter[F],
      interval: FiniteDuration
  ): Resource[F, MetricReader[F]] =
    for {
      metricProducers <- Resource.eval(Ref.empty[F, Vector[MetricProducer[F]]])
      reader = new PeriodicMetricReader(metricProducers, exporter)
      // start background exporter process
      _ <- reader.exportMetrics.delayBy(interval).foreverM[Unit].background
    } yield reader

}
