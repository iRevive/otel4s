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

import cats.effect.Ref
import cats.effect.Temporal
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.metrics.data.ExemplarData

import scala.concurrent.duration.FiniteDuration

private[exemplar] final class ReservoirCell[
    F[_]: Temporal,
    A,
    E <: ExemplarData
](
    stateRef: Ref[F, Option[ReservoirCell.State[A]]],
    lookup: TraceContextLookup
)(implicit make: ExemplarData.Make[A, E]) {

  def record(value: A, attributes: Attributes, context: Context): F[Unit] =
    for {
      now <- Temporal[F].realTime
      ctx <- Temporal[F].pure(lookup.get(context))
      _ <- stateRef.set(Some(ReservoirCell.State(value, attributes, ctx, now)))
    } yield ()

  def getAndReset(pointAttributes: Attributes): F[Option[E]] =
    stateRef.getAndSet(None).map { state =>
      state.map { s =>
        val attrs = filtered(s.attributes, pointAttributes)
        make.make(attrs, s.recordTime, s.traceContext, s.value)
      }
    }

  private def filtered(
      original: Attributes,
      metricPoint: Attributes
  ): Attributes =
    if (metricPoint.isEmpty) original
    else original.filterNot(a => metricPoint.contains(a.key))
}

private[exemplar] object ReservoirCell {

  private final case class State[A](
      value: A,
      attributes: Attributes,
      traceContext: Option[ExemplarData.TraceContext],
      recordTime: FiniteDuration
  )

  def create[F[_]: Temporal, A, E <: ExemplarData](
      lookup: TraceContextLookup
  )(implicit make: ExemplarData.Make[A, E]): F[ReservoirCell[F, A, E]] =
    for {
      stateRef <- Temporal[F].ref(Option.empty[State[A]])
    } yield new ReservoirCell(stateRef, lookup)

}
