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
import cats.effect.Temporal
import cats.effect.std.AtomicCell
import cats.effect.std.Console
import cats.effect.std.Random
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.metrics.Aggregation
import org.typelevel.otel4s.sdk.metrics.aggregation.Aggregator
import org.typelevel.otel4s.sdk.metrics.data.AggregationTemporality
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.data.PointData
import org.typelevel.otel4s.sdk.metrics.data.TimeWindow
import org.typelevel.otel4s.sdk.metrics.exemplar.ExemplarFilter
import org.typelevel.otel4s.sdk.metrics.exemplar.TraceContextLookup
import org.typelevel.otel4s.sdk.metrics.internal.InstrumentDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.MetricDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.exporter.RegisteredReader
import org.typelevel.otel4s.sdk.metrics.view.AttributesProcessor
import org.typelevel.otel4s.sdk.metrics.view.View

private final class DefaultSynchronous[F[_]: Monad: Console, A](
    reader: RegisteredReader[F],
    val metricDescriptor: MetricDescriptor,
    aggregator: DefaultSynchronous.SynchronousAggregator[F, A],
    attributesProcessor: AttributesProcessor,
    maxCardinality: Int,
    accumulators: AtomicCell[
      F,
      Map[Attributes, Aggregator.Accumulator[F, A, PointData]]
    ]
) extends MetricStorage.Synchronous[F, A] {

  private val aggregationTemporality =
    reader.reader.aggregationTemporalitySelector.select(
      metricDescriptor.sourceInstrument.instrumentType
    )

  def record(
      value: A,
      attributes: Attributes,
      context: Context
  ): F[Unit] =
    for {
      handle <- getHandle(attributes, context)
      _ <- handle.record(value, attributes, context)
    } yield ()

  def collect(
      resource: TelemetryResource,
      scope: InstrumentationScope,
      timeWindow: TimeWindow
  ): F[Option[MetricData]] = {
    val isDelta = aggregationTemporality == AggregationTemporality.Delta
    val reset = isDelta
    val getStart =
      if (isDelta) reader.lastCollectTimestamp
      else Monad[F].pure(timeWindow.start)

    val getHandlers =
      if (reset) accumulators.getAndSet(Map.empty) else accumulators.get

    for {
      start <- getStart
      handlers <- getHandlers
      window = TimeWindow(start, timeWindow.end)
      points <- handlers.toVector.traverse { case (attributes, handle) =>
        handle.aggregate(window, attributes, reset)
      }
      data <- aggregator.toMetricData(
        resource,
        scope,
        metricDescriptor,
        points.flatten,
        aggregationTemporality
      )
    } yield Some(data)
  }

  private def getHandle(
      attributes: Attributes,
      context: Context
  ): F[Aggregator.Accumulator[F, A, PointData]] =
    accumulators.evalModify { map =>
      val attrs = attributesProcessor.process(attributes, context)

      def createAccumulator =
        for {
          accumulator <- aggregator.createAccumulator
        } yield (map.updated(attrs, accumulator), accumulator)

      map.get(attrs) match {
        case Some(handle) =>
          Monad[F].pure((map, handle))

        case None =>
          if (map.sizeIs >= maxCardinality) {
            cardinalityWarning >> map
              .get(attributes.added(MetricStorage.OverflowAttribute))
              .fold(createAccumulator)(v => Monad[F].pure((map, v)))
          } else {
            createAccumulator
          }
      }
    }

  private def cardinalityWarning: F[Unit] =
    Console[F].errorln(
      s"Instrument [${metricDescriptor.sourceInstrument.name}] has exceeded the maximum allowed cardinality [$maxCardinality]"
    )

}

object DefaultSynchronous {

  private type SynchronousAggregator[F[_], A] =
    Aggregator.Synchronous[F, A] {
      type Point = PointData
    }

  def create[F[_]: Temporal: Console: Random, A: MeasurementValue: Numeric](
      reader: RegisteredReader[F],
      view: Option[View],
      instrumentDescriptor: InstrumentDescriptor.Synchronous,
      exemplarFilter: ExemplarFilter,
      traceContextLookup: TraceContextLookup,
      aggregation: Aggregation.Synchronous
  ): F[MetricStorage.Synchronous[F, A]] = {
    val descriptor = MetricDescriptor(view, instrumentDescriptor)

    val aggregator: Aggregator.Synchronous[F, A] =
      Aggregator.synchronous(
        aggregation,
        instrumentDescriptor,
        exemplarFilter,
        traceContextLookup
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

    AtomicCell[F]
      .of(Map.empty[Attributes, Aggregator.Accumulator[F, A, PointData]])
      .map { accumulators =>
        new DefaultSynchronous(
          reader,
          descriptor,
          aggregator.asInstanceOf[SynchronousAggregator[F, A]],
          attributesProcessor,
          cardinalityLimit - 1,
          accumulators
        )
      }
  }

}
