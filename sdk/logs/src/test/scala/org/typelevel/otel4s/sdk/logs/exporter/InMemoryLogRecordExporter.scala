/*
 * Copyright 2025 Typelevel
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

package org.typelevel.otel4s.sdk.logs.exporter

import cats.Foldable
import cats.Monad
import cats.effect.Concurrent
import cats.effect.std.Queue
import cats.syntax.foldable._
import cats.syntax.functor._
import org.typelevel.otel4s.sdk.logs.data.LogRecordData

final class InMemoryLogRecordExporter[F[_]: Monad] private (
    queue: Queue[F, LogRecordData]
) extends LogRecordExporter[F] {
  val name: String = "InMemoryLogRecordExporter"

  def exportLogRecords[G[_]: Foldable](logs: G[LogRecordData]): F[Unit] =
    logs.traverse_(log => queue.offer(log))

  def flush: F[Unit] =
    Monad[F].unit

  def finishedLogs: F[List[LogRecordData]] =
    queue.tryTakeN(None)

  def reset: F[Unit] =
    queue.tryTakeN(None).void
}

object InMemoryLogRecordExporter {

  def create[F[_]: Concurrent](capacity: Option[Int]): F[InMemoryLogRecordExporter[F]] =
    for {
      queue <- capacity.fold(Queue.unbounded[F, LogRecordData])(Queue.bounded(_))
    } yield new InMemoryLogRecordExporter[F](queue)

}