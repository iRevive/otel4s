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

package org.typelevel.otel4s
package oteljava.testkit

import cats.effect.Async
import cats.effect.Resource
import io.opentelemetry.context.propagation.{TextMapPropagator => JTextMapPropagator}
import io.opentelemetry.sdk.logs.SdkLoggerProviderBuilder
import io.opentelemetry.sdk.metrics.SdkMeterProviderBuilder
import io.opentelemetry.sdk.trace.SdkTracerProviderBuilder
import org.typelevel.otel4s.baggage.BaggageManager
import org.typelevel.otel4s.context.LocalProvider
import org.typelevel.otel4s.context.propagation.ContextPropagators
import org.typelevel.otel4s.logs.LoggerProvider
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.oteljava.baggage.BaggageManagerImpl
import org.typelevel.otel4s.oteljava.context.Context
import org.typelevel.otel4s.oteljava.context.LocalContext
import org.typelevel.otel4s.oteljava.context.LocalContextProvider
import org.typelevel.otel4s.oteljava.testkit.logs.FromLogRecordData
import org.typelevel.otel4s.oteljava.testkit.logs.LogsTestkit
import org.typelevel.otel4s.oteljava.testkit.metrics.FromMetricData
import org.typelevel.otel4s.oteljava.testkit.metrics.MetricsTestkit
import org.typelevel.otel4s.oteljava.testkit.trace.FromSpanData
import org.typelevel.otel4s.oteljava.testkit.trace.TracesTestkit
import org.typelevel.otel4s.trace.TracerProvider

sealed abstract class OtelJavaTestkit[F[_]] private (implicit
    val localContext: LocalContext[F]
) extends Otel4s.Unsealed[F]
    with LogsTestkit[F]
    with MetricsTestkit.Unsealed[F]
    with TracesTestkit.Unsealed[F] {

  type Ctx = Context

  val baggageManager: BaggageManager[F] = BaggageManagerImpl.fromLocal

  override def toString: String =
    s"OtelJavaTestkit{meterProvider=$meterProvider, tracerProvider=$tracerProvider, propagators=$propagators}"
}

object OtelJavaTestkit {

  /** Creates [[OtelJavaTestkit]] that keeps spans and metrics in-memory.
    *
    * @param customizeMeterProviderBuilder
    *   the customization of the meter provider builder
    *
    * @param customizeTracerProviderBuilder
    *   the customization of the tracer provider builder
    *
    * @param customizeLoggerProviderBuilder
    *   the customization of the logger provider builder
    *
    * @param textMapPropagators
    *   the propagators to use
    */
  def inMemory[F[_]: Async: LocalContextProvider](
      customizeMeterProviderBuilder: SdkMeterProviderBuilder => SdkMeterProviderBuilder = identity,
      customizeTracerProviderBuilder: SdkTracerProviderBuilder => SdkTracerProviderBuilder = identity,
      customizeLoggerProviderBuilder: SdkLoggerProviderBuilder => SdkLoggerProviderBuilder = identity,
      textMapPropagators: Iterable[JTextMapPropagator] = Nil
  ): Resource[F, OtelJavaTestkit[F]] =
    Resource.eval(LocalProvider[F, Context].local).flatMap { implicit local =>
      for {
        logs <- LogsTestkit.create(customizeLoggerProviderBuilder)
        metrics <- MetricsTestkit.create(customizeMeterProviderBuilder)
        traces <- TracesTestkit.inMemory(
          customizeTracerProviderBuilder,
          textMapPropagators
        )(Async[F], LocalProvider.fromLocal(local))
      } yield new Impl[F](logs, metrics, traces)
    }

  private final class Impl[F[_]](
      logs: LogsTestkit[F],
      metrics: MetricsTestkit[F],
      traces: TracesTestkit[F]
  ) extends OtelJavaTestkit[F]()(traces.localContext) {
    def loggerProvider: LoggerProvider[F, Context] = logs.loggerProvider
    def meterProvider: MeterProvider[F] = metrics.meterProvider
    def tracerProvider: TracerProvider[F] = traces.tracerProvider
    def propagators: ContextPropagators[Context] = traces.propagators
    def finishedSpans[A: FromSpanData]: F[List[A]] = traces.finishedSpans
    def resetSpans: F[Unit] = traces.resetSpans
    def collectMetrics[A: FromMetricData]: F[List[A]] = metrics.collectMetrics
    def collectLogs[A: FromLogRecordData]: F[List[A]] = logs.collectLogs
  }

}
