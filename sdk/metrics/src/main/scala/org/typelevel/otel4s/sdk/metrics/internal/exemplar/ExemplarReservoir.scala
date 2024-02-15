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

package org.typelevel.otel4s.sdk.metrics.internal.exemplar

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
import org.typelevel.otel4s.sdk.metrics.ExemplarFilter
import org.typelevel.otel4s.sdk.metrics.data.ExemplarData

private[internal] trait ExemplarReservoir[F[_], A, E <: ExemplarData] {
  def offer(value: A, attributes: Attributes, context: Context): F[Unit]
  def collectAndReset(attributes: Attributes): F[Vector[E]]
}

private[internal] object ExemplarReservoir {

  // size = availableProcessors
  def fixedSize[F[_]: Temporal: Random, A: Numeric, E <: ExemplarData](
      size: Int
  )(implicit make: ExemplarData.Make[A, E]): F[ExemplarReservoir[F, A, E]] =
    for {
      selector <- CellSelector.random[F, A, E]
      reservoir <- create(size, selector)
    } yield reservoir

  def histogramBucket[F[_]: Temporal, A: Numeric, E <: ExemplarData](
      boundaries: BucketBoundaries
  )(implicit make: ExemplarData.Make[A, E]): F[ExemplarReservoir[F, A, E]] =
    create(boundaries.length + 1, CellSelector.histogramBucket(boundaries))

  def filtered[F[_]: Applicative, A: MeasurementValue, E <: ExemplarData](
      filter: ExemplarFilter,
      original: ExemplarReservoir[F, A, E]
  ): ExemplarReservoir[F, A, E] =
    new ExemplarReservoir[F, A, E] {
      def offer(value: A, attributes: Attributes, context: Context): F[Unit] =
        original
          .offer(value, attributes, context)
          .whenA(
            filter.shouldSample(value, attributes, context)
          )

      def collectAndReset(attributes: Attributes): F[Vector[E]] =
        original.collectAndReset(attributes)
    }

  private def create[F[_]: Temporal, A, E <: ExemplarData](
      size: Int,
      cellSelector: CellSelector[F, A, E]
  )(implicit make: ExemplarData.Make[A, E]): F[ExemplarReservoir[F, A, E]] =
    for {
      cells <- ReservoirCell.create[F, A, E].replicateA(size)
      hasMeasurement <- Temporal[F].ref(false)
    } yield new FixedSize(cells.toVector, cellSelector, hasMeasurement)

  private final class FixedSize[F[_]: Monad, A, E <: ExemplarData](
      cells: Vector[ReservoirCell[F, A, E]],
      selector: CellSelector[F, A, E],
      hasMeasurement: Ref[F, Boolean],
  ) extends ExemplarReservoir[F, A, E] {

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

    def collectAndReset(attributes: Attributes): F[Vector[E]] = {
      def collect: F[Vector[E]] =
        for {
          results <- cells.traverse(cell => cell.getAndReset(attributes))
          _ <- selector.reset
          _ <- hasMeasurement.set(false)
        } yield results.flatten

      hasMeasurement.get.ifM(collect, Monad[F].pure(Vector.empty))
    }
  }
}
