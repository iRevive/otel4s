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

package org.typelevel.otel4s.sdk
package exporter
package otlp
package logs

import cats.effect.IO
import com.comcast.ip4s.IpAddress
import fs2.io.compression._
import munit._
import org.http4s.Headers
import org.http4s.headers._
import org.http4s.syntax.literals._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.effect.PropF

import scala.concurrent.duration._

class OtlpLogRecordExporterSuite extends CatsEffectSuite with ScalaCheckEffectSuite {

  private implicit val protocolArbitrary: Arbitrary[OtlpProtocol] =
    Arbitrary(
      Gen.oneOf(
        OtlpProtocol.httpJson,
        OtlpProtocol.httpProtobuf,
        OtlpProtocol.grpc
      )
    )

  private implicit val compressionArbitrary: Arbitrary[PayloadCompression] =
    Arbitrary(
      Gen.oneOf(
        PayloadCompression.gzip,
        PayloadCompression.none
      )
    )

  test("represent builder parameters in the name") {
    PropF.forAllF { (protocol: OtlpProtocol, compression: PayloadCompression) =>
      val expected =
        s"OtlpLogRecordExporter{client=OtlpClient{protocol=$protocol, " +
          "endpoint=https://localhost:4318/api/v1/logs, " +
          "timeout=5 seconds, " +
          s"compression=$compression, " +
          "headers={X-Forwarded-For: 127.0.0.1}}}"

      OtlpLogRecordExporter
        .builder[IO]
        .addHeaders(
          Headers(`X-Forwarded-For`(IpAddress.fromString("127.0.0.1")))
        )
        .withEndpoint(uri"https://localhost:4318/api/v1/logs")
        .withTimeout(5.seconds)
        .withProtocol(protocol)
        .withCompression(compression)
        .build
        .use { exporter =>
          IO(assertEquals(exporter.name, expected))
        }
    }
  }

  test("change endpoint according to the protocol") {
    PropF.forAllF { (protocol: OtlpProtocol) =>
      val endpoint = protocol match {
        case _: OtlpProtocol.Http =>
          "http://localhost:4318/v1/logs"

        case OtlpProtocol.Grpc =>
          "http://localhost:4317/opentelemetry.proto.collector.logs.v1.LogsService/Export"
      }

      val expected =
        s"OtlpLogRecordExporter{client=OtlpClient{protocol=$protocol, " +
          s"endpoint=$endpoint, " +
          "timeout=10 seconds, " +
          "compression=none, " +
          "headers={}}}"

      OtlpLogRecordExporter
        .builder[IO]
        .withProtocol(protocol)
        .build
        .use { exporter =>
          IO(assertEquals(exporter.name, expected))
        }
    }
  }

}