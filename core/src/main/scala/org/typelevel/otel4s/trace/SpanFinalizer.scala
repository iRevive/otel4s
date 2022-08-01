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
package trace

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.Resource
import cats.syntax.applicative._
import cats.syntax.foldable._

sealed trait SpanFinalizer extends Product with Serializable

object SpanFinalizer {

  type Strategy = PartialFunction[Resource.ExitCase, SpanFinalizer]

  object Strategy {
    def empty: Strategy = PartialFunction.empty

    def reportAbnormal: Strategy = {
      case Resource.ExitCase.Errored(e) =>
        SpanFinalizer.multiple(
          SpanFinalizer.RecordException(e),
          SpanFinalizer.SetStatus(Status.Error, None)
        )

      case Resource.ExitCase.Canceled =>
        SpanFinalizer.SetStatus(Status.Error, Some("canceled"))
    }
  }

  final case class RecordException(throwable: Throwable) extends SpanFinalizer

  final case class SetStatus(status: Status, description: Option[String])
      extends SpanFinalizer

  final case class SetAttributes(attributes: Seq[Attribute[_]])
      extends SpanFinalizer

  final case class Multiple(finalizers: NonEmptyList[SpanFinalizer])
      extends SpanFinalizer

  def multiple(head: SpanFinalizer, tail: SpanFinalizer*): Multiple =
    Multiple(NonEmptyList.of(head, tail: _*))

  private[otel4s] def run[F[_]: Applicative](
      backend: Span.Backend[F],
      finalizer: SpanFinalizer
  ): F[Unit] = {

    def loop(input: SpanFinalizer): F[Unit] =
      input match {
        case SpanFinalizer.RecordException(e) =>
          backend.recordException(e)

        case SpanFinalizer.SetStatus(status, description) =>
          description match {
            case Some(desc) => backend.setStatus(status, desc)
            case None       => backend.setStatus(status)
          }

        case SpanFinalizer.SetAttributes(attributes) =>
          backend.setAttributes(attributes: _*)

        case SpanFinalizer.Multiple(finalizers) =>
          finalizers.traverse_(strategy => loop(strategy))
      }

    loop(finalizer).whenA(backend.meta.isEnabled)
  }

}