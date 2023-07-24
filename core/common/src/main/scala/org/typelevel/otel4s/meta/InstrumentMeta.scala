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

package org.typelevel.otel4s.meta

import cats.Applicative
import cats.data.OptionT
import cats.~>

trait InstrumentMeta[F[_]] {

  /** Indicates whether instrumentation is enabled or not.
    */
  def isEnabled: Boolean

  /** A no-op effect.
    */
  def unit: F[Unit]

}

object InstrumentMeta {

  def enabled[F[_]: Applicative]: InstrumentMeta[F] =
    make(enabled = true)

  def disabled[F[_]: Applicative]: InstrumentMeta[F] =
    make(enabled = false)

  def liftOptionT[F[_]: Applicative](
      meta: InstrumentMeta[F]
  ): InstrumentMeta[OptionT[F, *]] =
    new InstrumentMeta[OptionT[F, *]] {
      def isEnabled: Boolean = meta.isEnabled
      def unit: OptionT[F, Unit] = OptionT.liftF(meta.unit)
    }

  implicit final class InstrumentMetaSyntax[F[_]](
      private val m: InstrumentMeta[F]
  ) extends AnyVal {

    def mapK[G[_]](fk: F ~> G): InstrumentMeta[G] =
      new InstrumentMeta[G] {
        def isEnabled: Boolean = m.isEnabled
        def unit: G[Unit] = fk(m.unit)
      }

  }

  private def make[F[_]: Applicative](enabled: Boolean): InstrumentMeta[F] =
    new InstrumentMeta[F] { self =>
      val isEnabled: Boolean = enabled
      val unit: F[Unit] = Applicative[F].unit
    }

}
