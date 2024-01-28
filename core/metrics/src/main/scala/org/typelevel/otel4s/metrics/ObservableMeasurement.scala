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

package org.typelevel.otel4s.metrics

import cats.Foldable
import cats.Functor
import cats.syntax.foldable._
import cats.syntax.functor._
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.Attributes

/** A callback that allows to record measurements.
  *
  * @tparam F
  *   the higher-kinded type of a polymorphic effect
  *
  * @tparam A
  *   the type of the measurement
  */
trait ObservableMeasurement[F[_], A] {

  /** Records a value with a set of attributes.
    *
    * @param value
    *   the value to record
    *
    * @param attributes
    *   the set of attributes to associate with the value
    */
  final def record(value: A, attributes: Attribute[_]*): F[Unit] =
    record(value, Attributes.fromSpecific(attributes))

  /** Records a value with a set of attributes.
    *
    * @param value
    *   the value to record
    *
    * @param attributes
    *   the set of attributes to associate with the value
    */
  def record(value: A, attributes: Attributes): F[Unit]
}

object ObservableMeasurement {

  /** The source of the [[Measurement]].
    *
    * @tparam F
    *   the higher-kinded type of a polymorphic effect
    *
    * @tparam A
    *   the type of the measurement
    */
  sealed trait Source[F[_], A]

  object Source {

    /** Creates a [[Source]] from the given callback.
      *
      * The source is expected to abide by the following restrictions:
      *   - Short-living and (ideally) non-blocking
      *   - Run in a finite amount of time
      *   - Safe to call repeatedly, across multiple threads
      *
      * @example
      *   {{{
      * val completed: Ref[F, Long] = ???
      * val source: ObservableMeasurement.Source[F, Long] =
      *   ObservableMeasurement.Source.callback { cb =>
      *     completed.get.flatMap(total => cb.record(total))
      *   }
      *   }}}
      *
      * @param cb
      *   the callback which observes measurements when invoked
      *
      * @tparam A
      *   the type of the measurement
      */
    def callback[F[_], A](
        cb: ObservableMeasurement[F, A] => F[Unit]
    ): Source[F, A] =
      Callback(cb)

    /** Creates a [[Source]] from the given effect.
      *
      * The source is expected to abide by the following restrictions:
      *   - Short-living and (ideally) non-blocking
      *   - Run in a finite amount of time
      *   - Safe to call repeatedly, across multiple threads
      *
      * @example
      *   {{{
      * val completed: Ref[F, Long] = ???
      * val source: ObservableMeasurement.Source[F, Long] =
      *   ObservableMeasurement.Source.effect(
      *     completed.get.map(total => List(Measurement(total)))
      *   )
      *   }}}
      *
      * @param measurements
      *   the effect that produces measurements
      *
      * @tparam A
      *   the type of the measurement
      */
    def effect[F[_]: Functor, G[_]: Foldable, A](
        measurements: F[G[Measurement[A]]]
    ): Source[F, A] =
      Effect(measurements.map(_.toIterable))

    private[otel4s] final case class Callback[F[_], A](
        cb: ObservableMeasurement[F, A] => F[Unit]
    ) extends Source[F, A]

    private[otel4s] final case class Effect[F[_], A](
        measurements: F[Iterable[Measurement[A]]]
    ) extends Source[F, A]
  }

}
