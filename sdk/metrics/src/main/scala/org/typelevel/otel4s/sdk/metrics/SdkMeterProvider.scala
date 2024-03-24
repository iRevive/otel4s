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

import cats.Applicative
import cats.Monad
import cats.effect.Clock
import cats.effect.Temporal
import cats.effect.std.Console
import cats.effect.std.Random
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.typelevel.otel4s.metrics.MeterBuilder
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.context.AskContext
import org.typelevel.otel4s.sdk.internal.ComponentRegistry
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.exporter.CollectionRegistration
import org.typelevel.otel4s.sdk.metrics.exporter.MetricProducer
import org.typelevel.otel4s.sdk.metrics.exporter.MetricReader
import org.typelevel.otel4s.sdk.metrics.internal.exemplar.ExemplarFilter
import org.typelevel.otel4s.sdk.metrics.internal.exemplar.TraceContextLookup
import org.typelevel.otel4s.sdk.metrics.internal.exporter.RegisteredReader
import org.typelevel.otel4s.sdk.metrics.internal.view.RegisteredView
import org.typelevel.otel4s.sdk.metrics.internal.view.ViewRegistry

private final class SdkMeterProvider[F[_]: Applicative](
    componentRegistry: ComponentRegistry[F, SdkMeter[F]],
    resource: TelemetryResource,
    views: Vector[RegisteredView],
    readers: Vector[RegisteredReader[F]],
    producers: Vector[MetricProducer[F]]
) extends MeterProvider[F] {
  import SdkMeterProvider.DefaultMeterName

  def meter(name: String): MeterBuilder[F] =
    if (readers.isEmpty) {
      MeterBuilder.noop[F]
    } else {
      val meterName = if (name.trim.isEmpty) DefaultMeterName else name
      SdkMeterBuilder(componentRegistry, meterName)
    }

  override def toString: String =
    "SdkMeterProvider{" +
      s"resource=$resource, " +
      s"metricsReads=${readers.map(_.reader).mkString("[", ", ", "]")} " +
      s"metricsProducers=${producers.mkString("[", ", ", "]")} " +
      s"views=${views.mkString("[", ", ", "]")}" +
      "}"

}

object SdkMeterProvider {

  private val DefaultMeterName = "unknown"

  /** Builder for [[org.typelevel.otel4s.metrics.MeterProvider MeterProvider]].
    */
  sealed trait Builder[F[_]] {

    /** Sets a [[TelemetryResource]] to be attached to all metrics created by
      * [[org.typelevel.otel4s.metrics.Meter Meter]].
      *
      * @note
      *   on multiple subsequent calls, the resource from the last call will be
      *   retained.
      *
      * @param resource
      *   the [[TelemetryResource]] to use
      */
    def withResource(resource: TelemetryResource): Builder[F]

    /** Merges the given [[TelemetryResource]] with the current one.
      *
      * @note
      *   if both resources have different non-empty `schemaUrl`, the merge will
      *   fail.
      *
      * @see
      *   [[TelemetryResource.mergeUnsafe]]
      *
      * @param resource
      *   the [[TelemetryResource]] to merge the current one with
      */
    def addResource(resource: TelemetryResource): Builder[F]

    /** Sets an [[ExemplarFilter]] to be used by all metrics.
      *
      * @param filter
      *   the [[ExemplarFilter]] to register
      */
    def withExemplarFilter(filter: ExemplarFilter): Builder[F]

    /** Sets a
      * [[org.typelevel.otel4s.sdk.metrics.internal.exemplar.TraceContextLookup TraceContextLookup]]
      * to be used by exemplars.
      *
      * @param lookup
      *   the
      *   [[org.typelevel.otel4s.sdk.metrics.internal.exemplar.TraceContextLookup]]
      *   to use
      */
    def withTraceContextLookup(lookup: TraceContextLookup): Builder[F]

    /** Registers a [[View]] for the given [[InstrumentSelector]].
      *
      * [[View]] affects aggregation and export of the instruments that match
      * the given `selector`.
      *
      * @param selector
      *   the [[InstrumentSelector]] to filter instruments with
      *
      * @param view
      *   the [[View]] to register
      */
    def registerView(selector: InstrumentSelector, view: View): Builder[F]

    /** Registers a
      * [[org.typelevel.otel4s.sdk.metrics.exporter.MetricReader MetricReader]].
      *
      * @param reader
      *   the
      *   [[org.typelevel.otel4s.sdk.metrics.exporter.MetricReader MetricReader]]
      *   to register
      */
    def registerMetricReader(reader: MetricReader[F]): Builder[F]

    def registerMetricReader(
        reader: MetricReader[F],
        selector: CardinalityLimitSelector
    ): Builder[F]

    /** Registers a
      * [[org.typelevel.otel4s.sdk.metrics.exporter.MetricProducer MetricProducer]].
      *
      * @param producer
      *   the
      *   [[org.typelevel.otel4s.sdk.metrics.exporter.MetricProducer MetricProducer]]
      *   to register
      */
    def registerMetricProducer(producer: MetricProducer[F]): Builder[F]

    /** Creates [[org.typelevel.otel4s.metrics.MeterProvider MeterProvider]]
      * with the configuration of this builder.
      */
    def build: F[MeterProvider[F]]
  }

  /** Creates a new [[Builder]] with default configuration.
    */
  def builder[F[_]: Temporal: Random: Console: AskContext]: Builder[F] =
    BuilderImpl(
      resource = TelemetryResource.default,
      exemplarFilter = None,
      traceContextLookup = TraceContextLookup.noop,
      registeredViews = Vector.empty,
      metricReaders = Map.empty,
      metricProducers = Vector.empty
    )

  private final case class BuilderImpl[
      F[_]: Temporal: Random: Console: AskContext
  ](
      resource: TelemetryResource,
      exemplarFilter: Option[ExemplarFilter],
      traceContextLookup: TraceContextLookup,
      registeredViews: Vector[RegisteredView],
      metricReaders: Map[MetricReader[F], CardinalityLimitSelector],
      metricProducers: Vector[MetricProducer[F]]
  ) extends Builder[F] {

    def withResource(resource: TelemetryResource): Builder[F] =
      copy(resource = resource)

    def addResource(resource: TelemetryResource): Builder[F] =
      copy(resource = this.resource.mergeUnsafe(resource))

    def withExemplarFilter(filter: ExemplarFilter): Builder[F] =
      copy(exemplarFilter = Some(filter))

    def withTraceContextLookup(lookup: TraceContextLookup): Builder[F] =
      copy(traceContextLookup = lookup)

    def registerView(selector: InstrumentSelector, view: View): Builder[F] =
      copy(registeredViews =
        registeredViews :+ internal.view.RegisteredView(selector, view)
      )

    def registerMetricReader(reader: MetricReader[F]): Builder[F] =
      copy(metricReaders =
        metricReaders.updated(reader, CardinalityLimitSelector.default)
      )

    def registerMetricReader(
        reader: MetricReader[F],
        selector: CardinalityLimitSelector
    ): Builder[F] =
      copy(metricReaders = metricReaders.updated(reader, selector))

    def registerMetricProducer(producer: MetricProducer[F]): Builder[F] =
      copy(metricProducers = metricProducers :+ producer)

    def build: F[MeterProvider[F]] = {
      if (metricReaders.isEmpty) {
        Monad[F].pure(MeterProvider.noop)
      } else
        Clock[F].realTime.flatMap { now =>
          metricReaders.toVector
            .traverse { case (reader, limit) =>
              val registry = ViewRegistry(
                reader.defaultAggregationSelector,
                limit,
                registeredViews
              )

              RegisteredReader.create(reader, registry)
            }
            .flatMap { readers =>
              ComponentRegistry
                .create { scope =>
                  val filter = exemplarFilter.getOrElse(
                    ExemplarFilter.traceBased(traceContextLookup)
                  )

                  for {
                    state <- MeterSharedState.create(
                      resource,
                      scope,
                      now,
                      filter,
                      traceContextLookup,
                      readers
                    )
                  } yield new SdkMeter[F](state)
                }
                .flatMap { registry =>
                  readers
                    .traverse_ { reader =>
                      val producers =
                        metricProducers :+ new LeasedMetricProducer[F](
                          registry,
                          reader
                        )

                      for {
                        _ <- reader.reader.register(
                          new SdkCollectionRegistration[F](producers, resource)
                        )
                        _ <- reader.setLastCollectTimestamp(now)
                      } yield ()
                    }
                    .map { _ =>
                      new SdkMeterProvider(
                        registry,
                        resource,
                        registeredViews,
                        readers,
                        metricProducers
                      )
                    }
                }

            }

        }
    }
  }

  private final class LeasedMetricProducer[F[_]: Monad: Clock](
      registry: ComponentRegistry[F, SdkMeter[F]],
      reader: RegisteredReader[F]
  ) extends MetricProducer[F] {
    def produce(resource: TelemetryResource): F[Vector[MetricData]] =
      registry.components.flatMap { meters =>
        Clock[F].realTime.flatMap { now =>
          for {
            result <- meters.flatTraverse(_.collectAll(reader, now))
            _ <- reader.setLastCollectTimestamp(now)
          } yield result
        }
      }
  }

  private final class SdkCollectionRegistration[F[_]: Applicative](
      producers: Vector[MetricProducer[F]],
      resource: TelemetryResource
  ) extends CollectionRegistration[F] {
    def collectAllMetrics: F[Vector[MetricData]] =
      producers.flatTraverse(producer => producer.produce(resource))
  }

}
