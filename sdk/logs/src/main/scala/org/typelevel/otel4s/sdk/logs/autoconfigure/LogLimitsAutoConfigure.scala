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

package org.typelevel.otel4s.sdk.logs.autoconfigure

import cats.effect.MonadCancelThrow
import cats.effect.Resource
import org.typelevel.otel4s.sdk.autoconfigure.AutoConfigure
import org.typelevel.otel4s.sdk.autoconfigure.Config
import org.typelevel.otel4s.sdk.logs.LogLimits

/** Autoconfigures [[LogLimits]].
  *
  * The configuration options:
  * {{{
  * | System property                | Environment variable           | Description                                                 |
  * |-------------------------------|--------------------------------|-------------------------------------------------------------|
  * | otel.log.max.number.of.logs   | OTEL_LOG_MAX_NUMBER_OF_LOGS    | The maximum allowed number of logs. Default is `1`.         |
  * }}}
  *
  * @see
  *   [[https://opentelemetry.io/docs/languages/java/sdk/#loglimits]]
  *
  * @see
  *   [[https://opentelemetry.io/docs/languages/java/configuration/#properties-general]]
  */
private final class LogLimitsAutoConfigure[F[_]: MonadCancelThrow]
    extends AutoConfigure.WithHint[F, LogLimits](
      "LogLimits",
      LogLimitsAutoConfigure.ConfigKeys.All
    ) {

  import LogLimitsAutoConfigure.ConfigKeys

  def fromConfig(config: Config): Resource[F, LogLimits] = {
    def configure =
      Right(LogLimits.default)

    Resource.eval(MonadCancelThrow[F].fromEither(configure))
  }
}

private[sdk] object LogLimitsAutoConfigure {

  private object ConfigKeys {
    val MaxNumberOfLogs: Config.Key[Int] =
      Config.Key("otel.log.max.number.of.logs")

    val All: Set[Config.Key[_]] =
      Set(MaxNumberOfLogs)
  }

  /** Returns [[AutoConfigure]] that configures the [[LogLimits]].
    *
    * The configuration options:
    * {{{
    * | System property                | Environment variable           | Description                                                 |
    * |-------------------------------|--------------------------------|-------------------------------------------------------------|
    * | otel.log.max.number.of.logs   | OTEL_LOG_MAX_NUMBER_OF_LOGS    | The maximum allowed number of logs. Default is `1`.         |
    * }}}
    */
  def apply[F[_]: MonadCancelThrow]: AutoConfigure[F, LogLimits] =
    new LogLimitsAutoConfigure[F]

}
