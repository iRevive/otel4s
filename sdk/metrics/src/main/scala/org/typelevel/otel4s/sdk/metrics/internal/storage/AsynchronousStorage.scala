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

package org.typelevel.otel4s.sdk.metrics.internal.storage

import cats.Monad
import cats.effect.Concurrent
import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.std.Console
import cats.mtl.Ask
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.context.AskContext
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.metrics.Aggregation
import org.typelevel.otel4s.sdk.metrics.aggregation.Aggregator
import org.typelevel.otel4s.sdk.metrics.data.AggregationTemporality
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.data.TimeWindow
import org.typelevel.otel4s.sdk.metrics.internal.AsynchronousMeasurement
import org.typelevel.otel4s.sdk.metrics.internal.InstrumentDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.MetricDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.exporter.RegisteredReader
import org.typelevel.otel4s.sdk.metrics.view.AttributesProcessor
import org.typelevel.otel4s.sdk.metrics.view.View

import scala.concurrent.duration.FiniteDuration

/** Stores aggregated metrics for asynchronous instruments.
  */
private final class AsynchronousStorage[
    F[_]: Monad: Console: AskContext,
    A
] private (
    val reader: RegisteredReader[F],
    val metricDescriptor: MetricDescriptor,
    aggregationTemporality: AggregationTemporality,
    aggregator: Aggregator.Asynchronous[F, A],
    attributesProcessor: AttributesProcessor,
    maxCardinality: Int,
    collector: AsynchronousStorage.Collector[F, A]
) extends MetricStorage.Asynchronous[F, A] {

  def record(measurement: AsynchronousMeasurement[A]): F[Unit] =
    for {
      context <- Ask[F, Context].ask
      start <- collector.startTimestamp(measurement)
      points <- collector.currentPoints
      _ <- {
        val attributes =
          attributesProcessor.process(measurement.attributes, context)

        if (points.contains(attributes)) {
          Console[F].errorln(
            s"Instrument ${metricDescriptor.sourceInstrument.name} has recorded multiple values for the same attributes $attributes"
          )
        } else {
          val timeWindow = TimeWindow(start, measurement.timeWindow.end)
          if (points.sizeIs >= maxCardinality) {
            cardinalityWarning >> collector.record(
              measurement.copy(
                attributes = attributes.added(MetricStorage.OverflowAttribute),
                timeWindow = timeWindow
              )
            )
          } else {
            collector.record(
              measurement.copy(attributes = attributes, timeWindow = timeWindow)
            )
          }
        }
      }
    } yield ()

  def collect(
      resource: TelemetryResource,
      scope: InstrumentationScope,
      timeWindow: TimeWindow
  ): F[Option[MetricData]] =
    collector.collectPoints.flatMap { measurements =>
      aggregator
        .toMetricData(
          resource,
          scope,
          metricDescriptor,
          measurements,
          aggregationTemporality
        )
        .map(Some(_))
    }

  private def cardinalityWarning: F[Unit] =
    MetricStorage.cardinalityWarning(
      metricDescriptor.sourceInstrument,
      maxCardinality
    )
}

private object AsynchronousStorage {

  def create[
      F[_]: Temporal: Console: AskContext,
      A: MeasurementValue: Numeric
  ](
      reader: RegisteredReader[F],
      view: Option[View],
      instrumentDescriptor: InstrumentDescriptor.Asynchronous,
      aggregation: Aggregation.Asynchronous
  ): F[MetricStorage.Asynchronous[F, A]] = {
    val descriptor = MetricDescriptor(view, instrumentDescriptor)

    val aggregator: Aggregator.Asynchronous[F, A] =
      Aggregator.asynchronous(aggregation, instrumentDescriptor)

    val aggregationTemporality =
      reader.reader.aggregationTemporalitySelector.select(
        descriptor.sourceInstrument.instrumentType
      )

    val attributesProcessor =
      view.flatMap(_.attributesProcessor).getOrElse(AttributesProcessor.noop)

    val cardinalityLimit =
      view
        .flatMap(_.cardinalityLimit)
        .getOrElse(
          reader.reader.defaultCardinalityLimitSelector
            .select(instrumentDescriptor.instrumentType)
        )

    for {
      collector <- Collector.create[F, A](
        aggregationTemporality,
        reader,
        aggregator
      )
    } yield new AsynchronousStorage[F, A](
      reader,
      descriptor,
      aggregationTemporality,
      aggregator,
      attributesProcessor,
      cardinalityLimit - 1,
      collector
    )
  }

  private trait Collector[F[_], A] {
    def startTimestamp(m: AsynchronousMeasurement[A]): F[FiniteDuration]
    def record(measurement: AsynchronousMeasurement[A]): F[Unit]
    def currentPoints: F[Map[Attributes, AsynchronousMeasurement[A]]]
    def collectPoints: F[Vector[AsynchronousMeasurement[A]]]
  }

  private object Collector {

    def create[F[_]: Concurrent, A](
        aggregationTemporality: AggregationTemporality,
        reader: RegisteredReader[F],
        aggregator: Aggregator.Asynchronous[F, A]
    ): F[Collector[F, A]] =
      aggregationTemporality match {
        case AggregationTemporality.Delta => delta[F, A](reader, aggregator)
        case AggregationTemporality.Cumulative => cumulative[F, A]
      }

    private def delta[F[_]: Concurrent, A](
        reader: RegisteredReader[F],
        aggregator: Aggregator.Asynchronous[F, A]
    ): F[Collector[F, A]] =
      for {
        points <- Ref.of(Map.empty[Attributes, AsynchronousMeasurement[A]])
        lastPoints <- Ref.of(Map.empty[Attributes, AsynchronousMeasurement[A]])
      } yield new Delta(reader, aggregator, points, lastPoints)

    private def cumulative[F[_]: Concurrent, A]: F[Collector[F, A]] =
      for {
        points <- Ref.of(Map.empty[Attributes, AsynchronousMeasurement[A]])
      } yield new Cumulative(points)

    private final class Delta[F[_]: Monad, A](
        reader: RegisteredReader[F],
        aggregator: Aggregator.Asynchronous[F, A],
        pointsRef: Ref[F, Map[Attributes, AsynchronousMeasurement[A]]],
        lastPointsRef: Ref[F, Map[Attributes, AsynchronousMeasurement[A]]]
    ) extends Collector[F, A] {
      def startTimestamp(m: AsynchronousMeasurement[A]): F[FiniteDuration] =
        reader.lastCollectTimestamp

      def record(measurement: AsynchronousMeasurement[A]): F[Unit] =
        pointsRef.update(_.updated(measurement.attributes, measurement))

      def currentPoints: F[Map[Attributes, AsynchronousMeasurement[A]]] =
        pointsRef.get

      def collectPoints: F[Vector[AsynchronousMeasurement[A]]] =
        for {
          points <- pointsRef.get
          lastPoints <- lastPointsRef.getAndSet(points)
        } yield points.toVector.map { case (k, v) =>
          lastPoints.get(k) match {
            case Some(lastPoint) =>
              aggregator.diff(lastPoint, v)
            case None =>
              v
          }
        }
    }

    private final class Cumulative[F[_]: Monad, A](
        pointsRef: Ref[F, Map[Attributes, AsynchronousMeasurement[A]]]
    ) extends Collector[F, A] {

      def startTimestamp(m: AsynchronousMeasurement[A]): F[FiniteDuration] =
        Monad[F].pure(m.timeWindow.start)

      def record(measurement: AsynchronousMeasurement[A]): F[Unit] =
        pointsRef.update(_.updated(measurement.attributes, measurement))

      def currentPoints: F[Map[Attributes, AsynchronousMeasurement[A]]] =
        pointsRef.get

      def collectPoints: F[Vector[AsynchronousMeasurement[A]]] =
        pointsRef.getAndSet(Map.empty).map(_.values.toVector)
    }

  }

}
