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

import cats.effect.IO
import cats.effect.Resource
import cats.mtl.Ask
import munit.CatsEffectSuite
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.logs.LoggerProvider
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.autoconfigure.AutoConfigure
import org.typelevel.otel4s.sdk.autoconfigure.Config
import org.typelevel.otel4s.sdk.context.AskContext
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.logs.SdkLoggerProvider
import org.typelevel.otel4s.sdk.logs.exporter.LogRecordExporter
import org.typelevel.otel4s.semconv.attributes.ServiceAttributes

class LoggerProviderAutoConfigureSuite extends CatsEffectSuite {
  import LoggerProviderAutoConfigure.Customizer

  test("load default") {
    val config = Config.ofProps(Map.empty)

    val expected =
      s"SdkLoggerProvider{resource=${TelemetryResource.empty}, logRecordProcessor=NoopLogRecordProcessor}"

    configure(config) { provider =>
      IO(assertEquals(provider.toString, expected))
    }
  }

  test("use given resource") {
    val config = Config.ofProps(Map.empty)
    val resource = TelemetryResource.default

    val expected =
      s"SdkLoggerProvider{resource=$resource, logRecordProcessor=NoopLogRecordProcessor}"

    configure(config, resource = resource) { provider =>
      IO(assertEquals(provider.toString, expected))
    }
  }

  test("customize logger provider") {
    val config = Config.ofProps(Map.empty)
    val customResource = TelemetryResource(
      Attributes(ServiceAttributes.ServiceName("test-service"))
    )

    val expected =
      s"SdkLoggerProvider{resource=$customResource, logRecordProcessor=NoopLogRecordProcessor}"

    configure(
      config,
      customizer = (b, _) => b.withResource(customResource)
    ) { provider =>
      IO(assertEquals(provider.toString, expected))
    }
  }

  private def configure[A](
      config: Config,
      resource: TelemetryResource = TelemetryResource.empty,
      customizer: Customizer[SdkLoggerProvider.Builder[IO]] = (a, _) => a,
      exporterConfigurers: Set[AutoConfigure.Named[IO, LogRecordExporter[IO]]] = Set.empty
  )(f: LoggerProvider[IO] => IO[A]): IO[A] = {
    implicit val askContext: AskContext[IO] = Ask.const(Context.root)

    val autoConfigure = LoggerProviderAutoConfigure[IO](
      resource,
      customizer,
      exporterConfigurers
    )

    autoConfigure.configure(config).use(f)
  }
}
