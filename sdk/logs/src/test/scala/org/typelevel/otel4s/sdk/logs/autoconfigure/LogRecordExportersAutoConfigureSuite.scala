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
import munit.CatsEffectSuite
import org.typelevel.otel4s.sdk.autoconfigure.AutoConfigure
import org.typelevel.otel4s.sdk.autoconfigure.Config
import org.typelevel.otel4s.sdk.logs.exporter.LogRecordExporter

class LogRecordExportersAutoConfigureSuite extends CatsEffectSuite {

  test("load default (otlp)") {
    val config = Config.ofProps(Map.empty)

    val result = configure(config).attempt.map { result =>
      assert(
        result.isLeft,
        "Expected failure because otlp exporter is not registered"
      )
      assert(
        result.left.exists(_.getMessage.contains("otlp")),
        "Expected error message to contain 'otlp'"
      )
    }

    result
  }

  test("load none exporter") {
    val config = Config.ofProps(
      Map("otel.logs.exporter" -> "none")
    )

    configure(config).use { exporters =>
      IO {
        assertEquals(exporters.size, 1)
        assert(exporters.contains("none"), "Expected 'none' exporter")
        assertEquals(exporters("none").name, "NoopLogRecordExporter")
      }
    }
  }

  test("load console exporter") {
    val config = Config.ofProps(
      Map("otel.logs.exporter" -> "console")
    )

    configure(config).use { exporters =>
      IO {
        assertEquals(exporters.size, 1)
        assert(exporters.contains("console"), "Expected 'console' exporter")
        assertEquals(exporters("console").name, "ConsoleLogRecordExporter")
      }
    }
  }

  test("load multiple exporters") {
    val config = Config.ofProps(
      Map("otel.logs.exporter" -> "console,none")
    )

    configure(config).use { exporters =>
      IO {
        assertEquals(exporters.size, 2)
        assert(exporters.contains("console"), "Expected 'console' exporter")
        assert(exporters.contains("none"), "Expected 'none' exporter")
        assertEquals(exporters("console").name, "ConsoleLogRecordExporter")
        assertEquals(exporters("none").name, "NoopLogRecordExporter")
      }
    }
  }

  test("fail when none is used with other exporters") {
    val config = Config.ofProps(
      Map("otel.logs.exporter" -> "none,console")
    )

    val result = configure(config).attempt.map { result =>
      assert(
        result.isLeft,
        "Expected failure because 'none' is used with other exporters"
      )
      assert(
        result.left.exists(_.getMessage.contains("'none' along with other exporters")),
        "Expected error message to contain 'none' along with other exporters"
      )
    }

    result
  }

  private def configure(
      config: Config,
      configurers: Set[AutoConfigure.Named[IO, LogRecordExporter[IO]]] = Set.empty
  ): Resource[IO, Map[String, LogRecordExporter[IO]]] = {
    val autoConfigure = LogRecordExportersAutoConfigure[IO](configurers)
    autoConfigure.configure(config)
  }
}