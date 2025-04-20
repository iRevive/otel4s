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
import cats.Functor
import cats.syntax.functor._

trait LoggerBuilder[F[_]] {

  /** Assigns a version to the resulting Logger.
    *
    * @param version
    *   the version of the instrumentation scope
    */
  def withVersion(version: String): LoggerBuilder[F]

  /** Assigns an OpenTelemetry schema URL to the resulting Logger.
    *
    * @param schemaUrl
    *   the URL of the OpenTelemetry schema
    */
  def withSchemaUrl(schemaUrl: String): LoggerBuilder[F]

  /** Creates a [[Logger]] with the given `version` and `schemaUrl` (if any).
    */
  def get: F[Logger[F]]

  /** Modify the context `F` using an implicit [[KindTransformer]] from `F` to `G`.
    */
  def mapK[G[_]](implicit F: Functor[F], kt: KindTransformer[F, G]): LoggerBuilder[G] =
    new LoggerBuilder.MappedK(this)
}

object LoggerBuilder {

  /** Creates a no-op implementation of the [[LoggerBuilder]].
    *
    * A [[Logger]] has no-op implementation too.
    *
    * @tparam F
    *   the higher-kinded type of polymorphic effect
    */
  def noop[F[_]](implicit F: Applicative[F]): LoggerBuilder[F] =
    new LoggerBuilder[F] {
      def withVersion(version: String): LoggerBuilder[F] = this
      def withSchemaUrl(schemaUrl: String): LoggerBuilder[F] = this
      def get: F[Logger[F]] = F.pure(Logger.noop)
    }

  private class MappedK[F[_]: Functor, G[_]](
      builder: LoggerBuilder[F]
  )(implicit kt: KindTransformer[F, G])
      extends LoggerBuilder[G] {
    def withVersion(version: String): LoggerBuilder[G] =
      new MappedK(builder.withVersion(version))
    def withSchemaUrl(schemaUrl: String): LoggerBuilder[G] =
      new MappedK(builder.withSchemaUrl(schemaUrl))
    def get: G[Logger[G]] =
      kt.liftK(builder.get.map(_.mapK[G]))
  }
}
