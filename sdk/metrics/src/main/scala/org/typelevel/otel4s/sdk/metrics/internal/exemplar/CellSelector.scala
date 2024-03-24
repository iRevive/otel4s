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
import cats.effect.Concurrent
import cats.effect.std.Random
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.typelevel.otel4s.metrics.BucketBoundaries
import org.typelevel.otel4s.sdk.metrics.internal.utils.Adder

private[exemplar] trait CellSelector[F[_], A] {
  def select(
      cells: Vector[ReservoirCell[F, A]],
      value: A,
  ): F[Option[ReservoirCell[F, A]]]

  def reset: F[Unit]
}

private[exemplar] object CellSelector {

  def histogramBucket[F[_]: Applicative, A: Numeric](
      boundaries: BucketBoundaries
  ): CellSelector[F, A] =
    new CellSelector[F, A] {
      def select(
          cells: Vector[ReservoirCell[F, A]],
          value: A
      ): F[Option[ReservoirCell[F, A]]] =
        Applicative[F].pure(
          cells.lift(
            boundaries.bucketIndex(Numeric[A].toDouble(value))
          )
        )

      def reset: F[Unit] = Applicative[F].unit
    }

  def random[F[_]: Concurrent: Random, A: Numeric]: F[CellSelector[F, A]] =
    for {
      adder <- Adder.create[F, A]
    } yield new CellSelector[F, A] {
      def select(
          cells: Vector[ReservoirCell[F, A]],
          value: A
      ): F[Option[ReservoirCell[F, A]]] = {
        for {
          sum <- adder.sum(reset = false)
          count = Numeric[A].toInt(sum) + 1
          idx <- Random[F].nextIntBounded(if (count > 0) count else 1)
          _ <- adder.add(Numeric[A].one)
        } yield cells.lift(idx)
      }

      def reset: F[Unit] = adder.sum(reset = true).void // todo use reset
    }

}
