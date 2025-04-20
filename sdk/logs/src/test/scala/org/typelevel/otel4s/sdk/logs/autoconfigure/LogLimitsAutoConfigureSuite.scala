/*
 * Copyright 2025 Typelevel
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

import cats.effect.IO
import cats.syntax.either._
import munit.CatsEffectSuite
import org.typelevel.otel4s.sdk.autoconfigure.Config
import org.typelevel.otel4s.sdk.logs.LogLimits

class LogLimitsAutoConfigureSuite extends CatsEffectSuite {

  /*test("load from an empty config - load default") {
    val config = Config(Map.empty, Map.empty, Map.empty)

    LogLimitsAutoConfigure[IO]
      .configure(config)
      .use { limits =>
        IO(assertEquals(limits.maxNumberOfLogs, LogLimits.default.maxNumberOfLogs))
      }
  }

  test("load from the config (empty string) - load default") {
    val props = Map(
      "otel.log.max.number.of.logs" -> ""
    )

    val config = Config.ofProps(props)

    LogLimitsAutoConfigure[IO]
      .configure(config)
      .use { limits =>
        IO(assertEquals(limits.maxNumberOfLogs, LogLimits.default.maxNumberOfLogs))
      }
  }

  test("load from the config - use given value") {
    val props = Map(
      "otel.log.max.number.of.logs" -> "100"
    )

    val config = Config.ofProps(props)

    LogLimitsAutoConfigure[IO]
      .configure(config)
      .use { limits =>
        IO(assertEquals(limits.maxNumberOfLogs, 100))
      }
  }*/

  test("invalid config value - fail") {
    val config =
      Config.ofProps(Map("otel.log.max.number.of.logs" -> "not int"))
    val error =
      "Invalid value for property otel.log.max.number.of.logs=not int. Must be [Int]"

    LogLimitsAutoConfigure[IO]
      .configure(config)
      .evalMap(IO.println)
      .use_
      .attempt
      .map(_.leftMap(_.getMessage))
      .assertEquals(
        Left(
          s"""Cannot autoconfigure [LogLimits].
             |Cause: $error.
             |Config:
             |1) `otel.log.max.number.of.logs` - not int""".stripMargin
        )
      )
  }

}
