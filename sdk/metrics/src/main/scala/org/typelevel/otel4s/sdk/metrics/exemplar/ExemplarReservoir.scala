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

package org.typelevel.otel4s.sdk.metrics.exemplar

import cats.Applicative
import cats.Monad
import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.std.Random
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.BucketBoundaries
import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.context.Context

private[metrics] trait ExemplarReservoir[F[_], A] {
  def offer(value: A, attributes: Attributes, context: Context): F[Unit]
  def collectAndReset(attributes: Attributes): F[Vector[Exemplar[A]]]
}

private[metrics] object ExemplarReservoir {

  // size = availableProcessors
  def fixedSize[F[_]: Temporal: Random, A: Numeric](
      size: Int,
      lookup: TraceContextLookup
  ): F[ExemplarReservoir[F, A]] =
    for {
      selector <- CellSelector.random[F, A]
      reservoir <- create(size, selector, lookup)
    } yield reservoir

  def histogramBucket[F[_]: Temporal, A: Numeric](
      boundaries: BucketBoundaries,
      lookup: TraceContextLookup
  ): F[ExemplarReservoir[F, A]] =
    create(
      boundaries.length + 1,
      CellSelector.histogramBucket(boundaries),
      lookup
    )

  def filtered[F[_]: Applicative, A: MeasurementValue](
      filter: ExemplarFilter,
      original: ExemplarReservoir[F, A]
  ): ExemplarReservoir[F, A] =
    new ExemplarReservoir[F, A] {
      def offer(value: A, attributes: Attributes, context: Context): F[Unit] =
        original
          .offer(value, attributes, context)
          .whenA(
            filter.shouldSample(value, attributes, context)
          )

      def collectAndReset(attributes: Attributes): F[Vector[Exemplar[A]]] =
        original.collectAndReset(attributes)
    }

  private def create[F[_]: Temporal, A](
      size: Int,
      cellSelector: CellSelector[F, A],
      lookup: TraceContextLookup
  ): F[ExemplarReservoir[F, A]] =
    for {
      cells <- ReservoirCell.create[F, A](lookup).replicateA(size)
      hasMeasurement <- Temporal[F].ref(false)
    } yield new FixedSize(cells.toVector, cellSelector, hasMeasurement)

  private final class FixedSize[F[_]: Monad, A](
      cells: Vector[ReservoirCell[F, A]],
      selector: CellSelector[F, A],
      hasMeasurement: Ref[F, Boolean],
  ) extends ExemplarReservoir[F, A] {

    def offer(value: A, attributes: Attributes, context: Context): F[Unit] =
      selector.select(cells, value).flatMap {
        case Some(cell) =>
          for {
            _ <- cell.record(value, attributes, context)
            _ <- hasMeasurement.set(true)
          } yield ()

        case None =>
          Monad[F].unit
      }

    def collectAndReset(attributes: Attributes): F[Vector[Exemplar[A]]] = {
      def collect: F[Vector[Exemplar[A]]] =
        for {
          results <- cells.traverse(cell => cell.getAndReset(attributes))
          _ <- selector.reset
          _ <- hasMeasurement.set(false)
        } yield results.flatten

      hasMeasurement.get.ifM(collect, Monad[F].pure(Vector.empty))
    }
  }
}
