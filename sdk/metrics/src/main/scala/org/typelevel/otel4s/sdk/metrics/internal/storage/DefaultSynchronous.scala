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
import org.typelevel.otel4s.sdk.metrics.ExemplarFilter
import org.typelevel.otel4s.sdk.metrics.data.AggregationTemporality
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.data.PointData
import org.typelevel.otel4s.sdk.metrics.internal.AttributesProcessor
import org.typelevel.otel4s.sdk.metrics.internal.InstrumentDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.MetricDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.aggregation.Aggregator
import org.typelevel.otel4s.sdk.metrics.internal.exemplar.TraceContextLookup
import org.typelevel.otel4s.sdk.metrics.internal.exporter.RegisteredReader
import org.typelevel.otel4s.sdk.metrics.internal.storage.MetricStorage.Synchronous
import org.typelevel.otel4s.sdk.metrics.internal.view.RegisteredView

import scala.concurrent.duration.FiniteDuration

private final class DefaultSynchronous[F[_]: Monad: Console, A](
    reader: RegisteredReader[F],
    val metricDescriptor: MetricDescriptor,
    aggregator: Aggregator.Aux[F, A, PointData],
    attributesProcessor: AttributesProcessor,
    maxCardinality: Int,
    handlers: AtomicCell[
      F,
      Map[Attributes, Aggregator.Handle[F, A, PointData]]
    ]
) extends Synchronous[F, A] {

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
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration
  ): F[Option[MetricData]] = {
    val isDelta = aggregationTemporality == AggregationTemporality.Delta
    val reset = isDelta
    val getStart =
      if (isDelta) reader.getLastCollectTimestamp
      else Monad[F].pure(startTimestamp)

    val getHandlers =
      if (reset) handlers.getAndSet(Map.empty) else handlers.get

    for {
      start <- getStart
      handlers <- getHandlers
      points <- handlers.toVector.traverse { case (attributes, handle) =>
        handle.aggregate(start, collectTimestamp, attributes, reset)
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
  ): F[Aggregator.Handle[F, A, PointData]] =
    handlers.evalModify { map =>
      val attrs = attributesProcessor.process(attributes, context)

      def createHandle =
        for {
          handle <- aggregator.createHandle
        } yield (map.updated(attrs, handle), handle)

      map.get(attrs) match {
        case Some(handle) =>
          Monad[F].pure((map, handle))

        case None =>
          if (map.sizeIs >= maxCardinality) {
            cardinalityWarning >> map
              .get(attributes.updated(MetricStorage.OverflowAttribute))
              .fold(createHandle)(v => Monad[F].pure((map, v)))
          } else {
            createHandle
          }
      }
    }

  private def cardinalityWarning: F[Unit] =
    Console[F].errorln(
      s"Instrument [${metricDescriptor.sourceInstrument.name}] has exceeded the maximum allowed cardinality [$maxCardinality]"
    )

}

object DefaultSynchronous {

  def create[F[_]: Temporal: Console: Random, A: MeasurementValue: Numeric](
      reader: RegisteredReader[F],
      registeredView: RegisteredView,
      instrumentDescriptor: InstrumentDescriptor,
      exemplarFilter: ExemplarFilter,
      traceContextLookup: TraceContextLookup,
      aggregation: Aggregation.HasAggregator
  ): F[Synchronous[F, A]] = {
    val view = registeredView.view
    val descriptor = MetricDescriptor(view, instrumentDescriptor)

    val aggregator: Aggregator[F, A] =
      Aggregator.create(
        aggregation,
        instrumentDescriptor,
        exemplarFilter,
        traceContextLookup
      )

    AtomicCell[F]
      .of(Map.empty[Attributes, Aggregator.Handle[F, A, PointData]])
      .map { handlers =>
        new DefaultSynchronous(
          reader,
          descriptor,
          aggregator.asInstanceOf[Aggregator.Aux[F, A, PointData]],
          registeredView.viewAttributesProcessor,
          registeredView.cardinalityLimit - 1,
          handlers
        )
      }
  }

}
