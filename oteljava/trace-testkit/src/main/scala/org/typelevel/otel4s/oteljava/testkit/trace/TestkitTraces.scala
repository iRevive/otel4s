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

package org.typelevel.otel4s.oteljava.testkit.trace

import cats.effect.Async
import cats.effect.Resource
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.opentelemetry.context.propagation.{
  TextMapPropagator => JTextMapPropagator
}
import io.opentelemetry.sdk.common.CompletableResultCode
import io.opentelemetry.sdk.testing.exporter.InMemorySpanExporter
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.sdk.trace.SdkTracerProviderBuilder
import io.opentelemetry.sdk.trace.SpanProcessor
import io.opentelemetry.sdk.trace.data.SpanData
import io.opentelemetry.sdk.trace.export.SimpleSpanProcessor
import io.opentelemetry.sdk.trace.export.SpanExporter
import org.typelevel.otel4s.context.LocalProvider
import org.typelevel.otel4s.context.propagation.ContextPropagators
import org.typelevel.otel4s.oteljava.context.Context
import org.typelevel.otel4s.oteljava.context.LocalContextProvider
import org.typelevel.otel4s.oteljava.context.propagation.PropagatorConverters._
import org.typelevel.otel4s.oteljava.trace.TracerProviderImpl
import org.typelevel.otel4s.trace.TracerProvider

import scala.jdk.CollectionConverters._

trait TestkitTraces[F[_]] {

  /** The [[TracerProvider]].
    */
  def provider: TracerProvider[F]

  /** The list of finished spans.
    *
    * @note
    *   each invocation cleans up the internal buffer.
    */
  def finishedSpans: F[List[SpanData]]
}

object TestkitTraces {

  /** Creates [[TestkitTraces]] that keeps spans in-memory.
    *
    * @param customize
    *   the customization of the builder
    *
    * @param propagators
    *   the context propagators to use
    */
  def inMemory[F[_]: Async: LocalContextProvider](
      customize: SdkTracerProviderBuilder => SdkTracerProviderBuilder =
        identity,
      propagators: Seq[JTextMapPropagator] = Nil
  ): Resource[F, TestkitTraces[F]] = {
    def createExporter: F[InMemorySpanExporter] =
      Async[F].delay(InMemorySpanExporter.create())

    def createProcessor(exporter: SpanExporter): F[SpanProcessor] =
      Async[F].delay(SimpleSpanProcessor.create(exporter))

    def createTracerProvider(processor: SpanProcessor): F[SdkTracerProvider] =
      Async[F].delay {
        val builder = SdkTracerProvider.builder().addSpanProcessor(processor)
        customize(builder).build
      }

    val ctxPropagators = ContextPropagators.of[Context](
      JTextMapPropagator.composite(propagators.asJava).asScala
    )

    for {
      local <- Resource.eval(LocalProvider[F, Context].local)
      exporter <- Resource.fromAutoCloseable(createExporter)
      processor <- Resource.fromAutoCloseable(createProcessor(exporter))
      provider <- Resource.fromAutoCloseable(createTracerProvider(processor))
    } yield new Impl(
      TracerProviderImpl.local(provider, ctxPropagators)(implicitly, local),
      processor,
      exporter
    )
  }

  private final class Impl[F[_]: Async](
      val provider: TracerProvider[F],
      processor: SpanProcessor,
      exporter: InMemorySpanExporter
  ) extends TestkitTraces[F] {
    def finishedSpans: F[List[SpanData]] =
      for {
        _ <- asyncFromCompletableResultCode(
          Async[F].delay(processor.forceFlush())
        )
        result <- Async[F].delay(exporter.getFinishedSpanItems.asScala.toList)
      } yield result
  }

  private def asyncFromCompletableResultCode[F[_]](
      codeF: F[CompletableResultCode],
      msg: => Option[String] = None
  )(implicit F: Async[F]): F[Unit] =
    F.flatMap(codeF)(code =>
      F.async[Unit](cb =>
        F.delay {
          code.whenComplete(() =>
            if (code.isSuccess())
              cb(Either.unit)
            else
              cb(
                Left(
                  new RuntimeException(
                    msg.getOrElse(
                      "OpenTelemetry SDK async operation failed"
                    )
                  )
                )
              )
          )
          None
        }
      )
    )

}
