package org.typelevel.otel4s.sdk.metrics.internal.storage

import cats.Monad
import cats.effect.Temporal
import cats.effect.std.{AtomicCell, Random}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.metrics.{Aggregation, ExemplarFilter, RegisteredReader, RegisteredView}
import org.typelevel.otel4s.sdk.metrics.data.{AggregationTemporality, MetricData, PointData}
import org.typelevel.otel4s.sdk.metrics.internal.{AttributesProcessor, InstrumentDescriptor, MetricDescriptor}
import org.typelevel.otel4s.sdk.metrics.internal.aggregation.Aggregator
import org.typelevel.otel4s.sdk.metrics.internal.storage.MetricStorage.Synchronous

import scala.concurrent.duration.FiniteDuration

private final class DefaultSynchronous[F[_]: Monad, A](
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
    reader.reader.aggregationTemporality(
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
      map.get(attrs) match {
        case Some(handle) =>
          Monad[F].pure((map, handle))

        case None =>
          // todo: check cardinality
          for {
            handle <- aggregator.createHandle
          } yield (map.updated(attrs, handle), handle)
      }
    }

}

object DefaultSynchronous {

  def create[F[_]: Temporal: Random, A: MeasurementValue: Numeric](
      reader: RegisteredReader[F],
      registeredView: RegisteredView,
      instrumentDescriptor: InstrumentDescriptor,
      exemplarFilter: ExemplarFilter,
      aggregation: Aggregation.HasAggregator
  ): F[Synchronous[F, A]] = {
    val view = registeredView.view
    val descriptor = MetricDescriptor(view, instrumentDescriptor)

    val aggregator: Aggregator[F, A] =
      Aggregator.create(
        aggregation,
        instrumentDescriptor,
        exemplarFilter
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
