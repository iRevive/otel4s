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

package org.typelevel.otel4s
package logs

import cats.Applicative

/** The entry point into a log pipeline.
  *
  * @note
  *   the `Logger` is intended to be used only for bridging logs from other log frameworks into OpenTelemetry and is
  *   '''NOT a replacement''' for logging API.
  */
trait Logger[F[_], Ctx] {

  def logRecordBuilder: LogRecordBuilder[F, Ctx]

  /** Modify the context `F` using an implicit [[KindTransformer]] from `F` to `G`.
    */
  def mapK[G[_]](implicit kt: KindTransformer[F, G]): Logger[G, Ctx] =
    new Logger.MappedK(this)
}

object Logger {
  def apply[F[_], Ctx](implicit ev: Logger[F, Ctx]): Logger[F, Ctx] = ev

  /** Creates a no-op implementation of the [[Logger]].
    *
    * All logging operations are no-op.
    *
    * @tparam F
    *   the higher-kinded type of polymorphic effect
    */
  def noop[F[_]: Applicative, Ctx]: Logger[F, Ctx] =
    new Logger[F, Ctx] {
      val logRecordBuilder: LogRecordBuilder[F, Ctx] = LogRecordBuilder.noop[F, Ctx]
    }

  /** Implementation for [[Logger.mapK]]. */
  private class MappedK[F[_], G[_], Ctx](
      logger: Logger[F, Ctx]
  )(implicit kt: KindTransformer[F, G])
      extends Logger[G, Ctx] {
    def logRecordBuilder: LogRecordBuilder[G, Ctx] = logger.logRecordBuilder.mapK
  }

  object Implicits {
    implicit def noop[F[_]: Applicative, Ctx]: Logger[F, Ctx] = Logger.noop
  }
}
