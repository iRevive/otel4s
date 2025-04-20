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

package org.typelevel.otel4s.logs

import cats.effect.IO
import cats.effect.kernel.Outcome
import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.syntax.flatMap._
import munit.CatsEffectSuite
import org.typelevel.otel4s.meta.InstrumentMeta

class LoggerProviderSuite extends CatsEffectSuite {

  test("LoggerProvider.noop returns a no-op LoggerProvider") {
    val provider = LoggerProvider.noop[IO]
    assertEquals(provider.toString, "LoggerProvider.Noop")
  }

  test("LoggerProvider.noop.get returns a no-op Logger") {
    val provider = LoggerProvider.noop[IO]
    provider.get("test").flatMap { logger =>
      assertEquals(logger.meta, InstrumentMeta.disabled[IO])
      logger.log(Severity.Info.level1, "test message")
    }
  }

  test("LoggerProvider.noop.logger returns a no-op LoggerBuilder") {
    val provider = LoggerProvider.noop[IO]
    val builder = provider.logger("test")
    builder.withVersion("1.0.0").withSchemaUrl("https://example.com").get.flatMap { logger =>
      assertEquals(logger.meta, InstrumentMeta.disabled[IO])
      logger.log(Severity.Info.level1, "test message")
    }
  }

  test("LoggerProvider.mapK transforms the effect type") {
    val provider = LoggerProvider.noop[IO]
    val mappedProvider = provider.mapK[IO]
    mappedProvider.get("test").flatMap { logger =>
      assertEquals(logger.meta, InstrumentMeta.disabled[IO])
      logger.log(Severity.Info.level1, "test message")
    }
  }
}
