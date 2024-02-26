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

package org.typelevel.otel4s.sdk.metrics.internal.exporter

import cats.effect.Concurrent
import cats.effect.Ref
import cats.syntax.functor._
import org.typelevel.otel4s.sdk.metrics.exporter.MetricReader
import org.typelevel.otel4s.sdk.metrics.internal.view.ViewRegistry

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

trait RegisteredReader[F[_]] {
  def reader: MetricReader[F]
  def viewRegistry: ViewRegistry
  def getLastCollectTimestamp: F[FiniteDuration]
  def setLastCollectTimestamp(timestamp: FiniteDuration): F[Unit]
}

object RegisteredReader {
  def create[F[_]: Concurrent](
      reader: MetricReader[F],
      viewRegistry: ViewRegistry
  ): F[RegisteredReader[F]] =
    Concurrent[F].ref(Duration.Zero).map { ref =>
      new Impl(ref, reader, viewRegistry)
    }

  private final class Impl[F[_]](
      lastCollectTimestamp: Ref[F, FiniteDuration],
      val reader: MetricReader[F],
      val viewRegistry: ViewRegistry
  ) extends RegisteredReader[F] {

    def getLastCollectTimestamp: F[FiniteDuration] =
      lastCollectTimestamp.get

    def setLastCollectTimestamp(timestamp: FiniteDuration): F[Unit] =
      lastCollectTimestamp.set(timestamp)
  }
}
