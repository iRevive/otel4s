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
import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.std.Console
import cats.effect.std.Random
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
import org.typelevel.otel4s.sdk.metrics.ExemplarFilter
import org.typelevel.otel4s.sdk.metrics.RegisteredReader
import org.typelevel.otel4s.sdk.metrics.RegisteredView
import org.typelevel.otel4s.sdk.metrics.data.AggregationTemporality
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.internal.AttributesProcessor
import org.typelevel.otel4s.sdk.metrics.internal.InstrumentDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.Measurement
import org.typelevel.otel4s.sdk.metrics.internal.MetricDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.aggregation.Aggregator
import org.typelevel.otel4s.sdk.metrics.internal.storage.MetricStorage.Asynchronous

import scala.concurrent.duration.FiniteDuration

private final class DefaultAsynchronous[
    F[_]: Monad: Console: AskContext,
    A: Numeric
](
    val reader: RegisteredReader[F],
    val metricDescriptor: MetricDescriptor,
    aggregator: Aggregator[F, A],
    attributesProcessor: AttributesProcessor,
    maxCardinality: Int,
    points: Ref[F, Map[Attributes, Measurement[A]]],
    lastPoints: Ref[F, Map[Attributes, Measurement[A]]]
) extends Asynchronous[F, A] {

  private val aggregationTemporality =
    reader.reader.aggregationTemporality(
      metricDescriptor.sourceInstrument.instrumentType
    )

  def record(m: Measurement[A]): F[Unit] = {

    def doRecord(measurement: Measurement[A]) = {
      points.update(_.updated(measurement.attributes, measurement))
    }

    for {
      context <- Ask[F, Context].ask
      attributes = attributesProcessor.process(m.attributes, context)
      start <-
        if (aggregationTemporality == AggregationTemporality.Delta)
          reader.getLastCollectTimestamp
        else
          Monad[F].pure(m.startTimestamp)
      measurement = m.withAttributes(attributes).withStartTimestamp(start)
      k <- points.get
      _ <- {
        if (k.contains(attributes)) {
          Console[F].errorln(
            s"Instrument ${metricDescriptor.sourceInstrument.name} has recorded multiple values for the same attributes ${attributes}"
          )
        } else {
          if (k.sizeIs >= maxCardinality) {
            Console[F].errorln(
              s"Instrument ${metricDescriptor.sourceInstrument.name} has exceeded the maximum allowed cardinality [$maxCardinality]"
            ) >> doRecord(
              measurement.withAttributes(
                attributes.updated("otel.metric.overflow", true)
              )
            )
          } else {
            doRecord(measurement)
          }
        }
      }
    } yield ()
  }

  def collect(
      resource: TelemetryResource,
      scope: InstrumentationScope,
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration
  ): F[Option[MetricData]] = {
    val delta: F[Vector[Measurement[A]]] = aggregationTemporality match {
      case AggregationTemporality.Delta =>
        for {
          points <- points.get
          lastPoints <- lastPoints.getAndSet(points)
        } yield {
          points.map { case (k, v) =>
            lastPoints.get(k) match {
              case Some(lastPoint) =>
                v.withValue(Numeric[A].minus(lastPoint.value, v.value))
              case None =>
                v
            }
          }.toVector
        }

      case AggregationTemporality.Cumulative =>
        points.getAndSet(Map.empty).map(_.values.toVector)
    }

    delta.flatMap { measurements =>
      val points = measurements.flatMap { measurement =>
        aggregator.toPointData(
          measurement.startTimestamp,
          measurement.collectTimestamp,
          measurement.attributes,
          measurement.value
        )
      }

      aggregator
        .toMetricData(
          resource,
          scope,
          metricDescriptor,
          points,
          aggregationTemporality
        )
        .map(Some(_))
    }
  }

}

private object DefaultAsynchronous {

  def create[
      F[_]: Temporal: Random: Console: AskContext,
      A: MeasurementValue: Numeric
  ](
      reader: RegisteredReader[F],
      registeredView: RegisteredView,
      instrumentDescriptor: InstrumentDescriptor,
      aggregation: Aggregation.HasAggregator
  ): F[Asynchronous[F, A]] = {
    val view = registeredView.view
    val descriptor = MetricDescriptor(view, instrumentDescriptor)

    val aggregator: Aggregator[F, A] =
      Aggregator.create(
        aggregation,
        instrumentDescriptor,
        ExemplarFilter.alwaysOff
      )

    Ref.of(Map.empty[Attributes, Measurement[A]]).flatMap { points =>
      Ref.of(Map.empty[Attributes, Measurement[A]]).map { lastPoints =>
        new DefaultAsynchronous[F, A](
          reader,
          descriptor,
          aggregator,
          registeredView.viewAttributesProcessor,
          registeredView.cardinalityLimit - 1,
          points,
          lastPoints
        )
      }
    }
  }

}
