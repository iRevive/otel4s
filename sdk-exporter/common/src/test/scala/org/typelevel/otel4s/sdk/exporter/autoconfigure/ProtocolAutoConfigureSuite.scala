package org.typelevel.otel4s.sdk.exporter.autoconfigure

import cats.effect.IO
import cats.syntax.either._
import munit.CatsEffectSuite
import org.typelevel.otel4s.sdk.autoconfigure.Config
import org.typelevel.otel4s.sdk.exporter.otlp.{HttpPayloadEncoding, Protocol}
import org.typelevel.otel4s.sdk.exporter.otlp.autoconfigure.ProtocolAutoConfigure

class ProtocolAutoConfigureSuite extends CatsEffectSuite {

  test("metrics - load from the config - empty config - load default") {
    val config = Config.ofProps(Map.empty)
    val expected = Protocol.Http(HttpPayloadEncoding.Protobuf)

    ProtocolAutoConfigure.metrics[IO]
      .configure(config)
      .use(protocol => IO(assertEquals(protocol, expected)))
  }

  test("metrics - load from the config - empty string - load default") {
    val config = Config.ofProps(
      Map(
        "otel.exporter.otlp.protocol" -> "",
        "otel.exporter.otlp.metrics.protocol" -> ""
      )
    )

    val expected = Protocol.Http(HttpPayloadEncoding.Protobuf)

    ProtocolAutoConfigure.metrics[IO]
      .configure(config)
      .use(protocol => IO(assertEquals(protocol, expected)))
  }
  
  test("metrics - load from the config - prioritize 'metrics' properties") {
    val config = Config.ofProps(
      Map(
        "otel.exporter.otlp.protocol" -> "http/protobuf",
        "otel.exporter.otlp.metrics.protocol" -> "http/json"
      )
    )

    val expected = Protocol.Http(HttpPayloadEncoding.Json)

    ProtocolAutoConfigure.metrics[IO]
      .configure(config)
      .use(protocol => IO(assertEquals(protocol, expected)))
  }

  test("metrics - load from the config - unknown protocol - fail") {
    val config = Config.ofProps(Map("otel.exporter.otlp.protocol" -> "grpc"))

    ProtocolAutoConfigure.metrics[IO]
      .configure(config)
      .use_
      .attempt
      .map(_.leftMap(_.getMessage))
      .assertEquals(
        Left("""Cannot autoconfigure [Protocol].
               |Cause: Unrecognized protocol [grpc]. Supported options [http/json, http/protobuf].
               |Config:
               |1) `otel.exporter.otlp.metrics.protocol` - N/A
               |2) `otel.exporter.otlp.protocol` - grpc""".stripMargin)
      )
  }
  
  test("traces - load from the config - empty config - load default") {
    val config = Config.ofProps(Map.empty)
    val expected = Protocol.Http(HttpPayloadEncoding.Protobuf)

    ProtocolAutoConfigure.traces[IO]
      .configure(config)
      .use(protocol => IO(assertEquals(protocol, expected)))
  }

  test("traces - load from the config - empty string - load default") {
    val config = Config.ofProps(
      Map(
        "otel.exporter.otlp.protocol" -> "",
        "otel.exporter.otlp.traces.protocol" -> ""
      )
    )

    val expected = Protocol.Http(HttpPayloadEncoding.Protobuf)

    ProtocolAutoConfigure.traces[IO]
      .configure(config)
      .use(protocol => IO(assertEquals(protocol, expected)))
  }
  
  test("traces - load from the config - prioritize 'traces' properties") {
    val config = Config.ofProps(
      Map(
        "otel.exporter.otlp.protocol" -> "http/protobuf",
        "otel.exporter.otlp.traces.protocol" -> "http/json"
      )
    )

    val expected = Protocol.Http(HttpPayloadEncoding.Json)

    ProtocolAutoConfigure.traces[IO]
      .configure(config)
      .use(protocol => IO(assertEquals(protocol, expected)))
  }

  test("traces - load from the config - unknown protocol - fail") {
    val config = Config.ofProps(Map("otel.exporter.otlp.protocol" -> "grpc"))

    ProtocolAutoConfigure.traces[IO]
      .configure(config)
      .use_
      .attempt
      .map(_.leftMap(_.getMessage))
      .assertEquals(
        Left("""Cannot autoconfigure [Protocol].
               |Cause: Unrecognized protocol [grpc]. Supported options [http/json, http/protobuf].
               |Config:
               |1) `otel.exporter.otlp.protocol` - grpc
               |2) `otel.exporter.otlp.traces.protocol` - N/A""".stripMargin)
      )
  }

}
