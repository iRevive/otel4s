/*
 * Copyright 2022 Typelevel
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

package org.typelevel.otel4s.sdk.exporter.otlp.autoconfigure

import cats.effect.Async
import cats.effect.std.Console
import fs2.compression.Compression
import fs2.io.net.Network
import org.http4s.client.Client
import org.typelevel.otel4s.sdk.autoconfigure.ExporterAutoConfigure
import org.typelevel.otel4s.sdk.exporter.otlp.metrics.autoconfigure.OtlpMetricExporterAutoConfigure
import org.typelevel.otel4s.sdk.exporter.otlp.trace.autoconfigure.OtlpSpanExporterAutoConfigure

object OtlpExporterAutoConfigure {

  def apply[
      F[_]: Async: Network: Compression: Console
  ]: ExporterAutoConfigure[F] =
    ExporterAutoConfigure(
      OtlpMetricExporterAutoConfigure[F],
      OtlpSpanExporterAutoConfigure[F]
    )

  def customClient[
      F[_]: Async: Network: Compression: Console
  ](client: Client[F]): ExporterAutoConfigure[F] =
    ExporterAutoConfigure(
      OtlpMetricExporterAutoConfigure.customClient[F](client),
      OtlpSpanExporterAutoConfigure.customClient[F](client)
    )

}
