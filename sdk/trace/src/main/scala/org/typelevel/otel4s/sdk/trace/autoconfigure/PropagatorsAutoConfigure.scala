/*
 * Copyright 2023 Typelevel
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

package org.typelevel.otel4s.sdk.trace.autoconfigure

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.effect.Resource
import org.typelevel.otel4s.context.propagation.ContextPropagators
import org.typelevel.otel4s.context.propagation.TextMapPropagator
import org.typelevel.otel4s.sdk.autoconfigure.AutoConfigure
import org.typelevel.otel4s.sdk.autoconfigure.Config
import org.typelevel.otel4s.sdk.autoconfigure.ConfigurationError
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.trace.context.propagation.B3Propagator
import org.typelevel.otel4s.sdk.trace.context.propagation.W3CBaggagePropagator
import org.typelevel.otel4s.sdk.trace.context.propagation.W3CTraceContextPropagator

private final class PropagatorsAutoConfigure[F[_]: MonadThrow](
    extra: Set[AutoConfigure.Named[F, TextMapPropagator[Context]]]
) extends AutoConfigure.WithHint[F, ContextPropagators[Context]](
      "ContextPropagators",
      PropagatorsAutoConfigure.ConfigKeys.All
    ) {

  import PropagatorsAutoConfigure.ConfigKeys
  import PropagatorsAutoConfigure.Default

  private val configurers = {
    val default: Set[AutoConfigure.Named[F, TextMapPropagator[Context]]] = Set(
      constConfigure("tracecontext", W3CTraceContextPropagator.default),
      constConfigure("baggage", W3CBaggagePropagator.default),
      constConfigure("b3", B3Propagator.singleHeader),
      constConfigure("b3multi", B3Propagator.multipleHeaders),
    )

    default ++ extra
  }

  def fromConfig(config: Config): Resource[F, ContextPropagators[Context]] = {
    val values = config.getOrElse(ConfigKeys.Propagators, Set.empty[String])

    Resource.eval(MonadThrow[F].fromEither(values)).flatMap {
      case names if names.contains("none") && names.sizeIs > 1 =>
        Resource.raiseError(
          ConfigurationError(
            s"[${ConfigKeys.Propagators.name}] contains 'none' along with other propagators"
          ): Throwable
        )

      case names if names.contains("none") =>
        Resource.pure(ContextPropagators.noop)

      case names =>
        val requested = NonEmptyList.fromList(names.toList).getOrElse(Default)

        for {
          propagators <- requested.traverse(name => create(name, config))
        } yield ContextPropagators.of(propagators.toList: _*)
    }
  }

  private def create(
      name: String,
      config: Config
  ): Resource[F, TextMapPropagator[Context]] =
    configurers.find(_.name == name) match {
      case Some(configurer) =>
        configurer.configure(config)

      case None =>
        Resource.raiseError(
          ConfigurationError.unrecognized(
            ConfigKeys.Propagators.name,
            name,
            configurers.map(_.name)
          ): Throwable
        )
    }

  private def constConfigure[A](
      n: String,
      component: A
  ): AutoConfigure.Named[F, A] =
    new AutoConfigure.Named[F, A] {
      def name: String = n
      def configure(config: Config): Resource[F, A] =
        Resource.pure(component)
    }
}

private[sdk] object PropagatorsAutoConfigure {

  private object ConfigKeys {
    val Propagators: Config.Key[Set[String]] = Config.Key("otel.propagators")

    val All: Set[Config.Key[_]] = Set(ConfigKeys.Propagators)
  }

  private val Default = NonEmptyList.of("tracecontext", "baggage")

  def apply[F[_]: MonadThrow](
      extra: Set[AutoConfigure.Named[F, TextMapPropagator[Context]]]
  ): AutoConfigure[F, ContextPropagators[Context]] =
    new PropagatorsAutoConfigure[F](extra)

}