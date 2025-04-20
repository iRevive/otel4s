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

package org.typelevel.otel4s.sdk.logs

import cats.effect.IO
import cats.mtl.Ask
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.typelevel.otel4s.sdk.context.{AskContext, Context}
import org.typelevel.otel4s.sdk.logs.processor.LogRecordProcessor
import org.typelevel.otel4s.sdk.logs.scalacheck.Gens

class SdkLoggerProviderSuite extends CatsEffectSuite with ScalaCheckEffectSuite {

  private implicit val askContext: AskContext[IO] = Ask.const(Context.root)

  test("empty builder - return noop instance") {
    for {
      provider <- SdkLoggerProvider.builder[IO].build
    } yield assertEquals(provider.toString, "LoggerProvider.Noop")
  }

  test("reflect parameters in the toString") {
    PropF.forAllF(Gens.telemetryResource) { resource =>
      val processor = LogRecordProcessor.noop[IO]

      val expected =
        s"SdkLoggerProvider{resource=$resource, logRecordProcessor=$processor}"

      for {
        provider <- SdkLoggerProvider
          .builder[IO]
          .withResource(resource)
          .addLogRecordProcessor(processor)
          .build
      } yield assertEquals(provider.toString, expected)
    }
  }

}
