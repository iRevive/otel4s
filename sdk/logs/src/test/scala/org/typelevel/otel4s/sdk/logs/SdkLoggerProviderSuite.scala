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

package org.typelevel.otel4s.sdk.logs

import cats.Applicative
import cats.effect.IO
import cats.effect.std.Random
import cats.mtl.Ask
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF
import org.typelevel.otel4s.sdk.context.AskContext
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.logs.exporter.LogRecordExporter
import org.typelevel.otel4s.sdk.logs.scalacheck.Gens

class SdkLoggerProviderSuite extends CatsEffectSuite with ScalaCheckEffectSuite {

  private implicit val askContext: AskContext[IO] = Ask.const(Context.root)

  test("empty builder - return noop instance") {
    Random.scalaUtilRandom[IO].flatMap { implicit R: Random[IO] =>
      for {
        provider <- SdkLoggerProvider.builder[IO].build
      } yield assertEquals(provider.toString, "LoggerProvider.Noop")
    }
  }

  test("reflect parameters in the toString") {
    PropF.forAllF(Gens.telemetryResource) { resource =>
      val exporter = new EmptyLogRecordExporter[IO]

      val expected = {
        s"SdkLoggerProvider{resource=$resource, exporter=$exporter}"
      }

      Random.scalaUtilRandom[IO].flatMap { implicit R: Random[IO] =>
        for {
          provider <- SdkLoggerProvider
            .builder[IO]
            .withResource(resource)
            .withExporter(exporter)
            .build
        } yield assertEquals(provider.toString, expected)
      }
    }
  }

  private class EmptyLogRecordExporter[F[_]: Applicative] extends LogRecordExporter[F] {
    def name: String = "LogRecordExporter.Empty"
    def exportLogs[G[_]: cats.Foldable](logs: G[org.typelevel.otel4s.sdk.logs.data.LogRecordData]): F[Unit] = 
      Applicative[F].unit
    def flush: F[Unit] = Applicative[F].unit
    override def toString: String = "LogRecordExporter.Empty"
  }
}
