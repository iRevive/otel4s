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

package org.typelevel.otel4s
package sdk
package exporter.otlp
package logs

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import munit._
import org.scalacheck.Test
import org.typelevel.otel4s.logs.Severity
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.logs.data.LogRecordData
import scodec.bits.ByteVector

import scala.concurrent.duration._

class LogsProtoEncoderSuite extends ScalaCheckSuite {
  import LogsJsonCodecs._
  import LogsProtoEncoder._

  test("encode LogRecordData") {
    // todo: use gen

    // Create a simple LogRecordData instance for testing
    val resource = TelemetryResource.default
    val scope = InstrumentationScope("test-scope", None, None, Attributes.empty)
    val timestamp = 1.second
    val observedTimestamp = 2.second
    val severity = Some(Severity.Info.info1)
    val severityText = Some("INFO")
    val body = Some(Value.StringValue("test log message"))
    val attributes = Attributes(Attribute("key", "value"))
    val traceId = ByteVector.fromValidHex("aae6750d58ff8148fa33894599afaaf2")
    val spanId = ByteVector.fromValidHex("f676d76b0b3d4324")
    val traceContext = None

    val logRecord = LogRecordData(
      timestamp = Some(timestamp),
      observedTimestamp = observedTimestamp,
      traceContext = traceContext,
      severity = severity,
      severityText = severityText,
      body = body,
      attributes = attributes,
      instrumentationScope = scope,
      resource = resource
    )

    val expected = Json
      .obj(
        "timeUnixNano" := timestamp.toNanos.toString,
        "observedTimeUnixNano" := observedTimestamp.toNanos.toString,
        "severityNumber" := severity.map(_.severity).getOrElse(0),
        "severityText" := severityText.getOrElse(""),
        "body" := body.map(v => encodeValue(v)),
        "attributes" := attributes,
        // "droppedAttributesCount" := 0,
        // "traceId" := traceContext.map(_.traceId.toHex),
        // "spanId" := traceContext.map(_.spanId.toHex)
      )
      .dropNullValues
      .dropEmptyValues

    assertEquals(ProtoEncoder.toJson(logRecord), expected)
  }

  test("encode List[LogRecordData]") {
    // Create a simple list of LogRecordData instances for testing
    val resource = TelemetryResource.default
    val scope = InstrumentationScope("test-scope", None, None, Attributes.empty)
    val timestamp = 1.second
    val observedTimestamp = 2.second
    val severity = Some(Severity.Info.info1)
    val severityText = Some("INFO")
    val body = Some(Value.StringValue("test log message"))
    val attributes = Attributes(Attribute("key", "value"))
    val traceId = ByteVector.fromValidHex("aae6750d58ff8148fa33894599afaaf2")
    val spanId = ByteVector.fromValidHex("f676d76b0b3d4324")
    val traceContext = None // Some(SimpleTraceContext(traceId, spanId))

    val logRecord = LogRecordData(
      timestamp = Some(timestamp),
      observedTimestamp = observedTimestamp,
      traceContext = traceContext,
      severity = severity,
      severityText = severityText,
      body = body,
      attributes = attributes,
      instrumentationScope = scope,
      resource = resource
    )

    val logs = List(logRecord)

    val resourceLogs =
      logs.groupBy(_.resource).map { case (resource, resourceLogs) =>
        val scopeLogs: Iterable[Json] =
          resourceLogs
            .groupBy(_.instrumentationScope)
            .map { case (scope, logs) =>
              Json
                .obj(
                  "scope" := scope,
                  "logRecords" := logs.map(Encoder[LogRecordData].apply),
                  "schemaUrl" := scope.schemaUrl
                )
                .dropNullValues
            }

        Json
          .obj(
            "resource" := resource,
            "scopeLogs" := scopeLogs,
            "schemaUrl" := resource.schemaUrl
          )
          .dropNullValues
      }

    val expected = Json.obj("resourceLogs" := resourceLogs).dropEmptyValues

    assertEquals(
      ProtoEncoder.toJson(logs).noSpacesSortKeys,
      expected.noSpacesSortKeys
    )
  }

  private def encodeValue(value: Value): Json = {
    value match {
      case Value.StringValue(v)     => Json.obj("stringValue" := v)
      case Value.BooleanValue(v)    => Json.obj("boolValue" := v)
      case Value.LongValue(v)       => Json.obj("intValue" := v.toString)
      case Value.DoubleValue(v)     => Json.obj("doubleValue" := v)
      case Value.ByteArrayValue(v)  => Json.obj("bytesValue" := ByteVector(v).toBase64)
      case Value.ArrayValue(values) => Json.obj("arrayValue" := Json.obj("values" := values.map(encodeValue)))
      case Value.MapValue(values) =>
        Json.obj("kvlistValue" := Json.obj("values" := values.map { case (k, v) =>
          Json.obj("key" := k, "value" := encodeValue(v))
        }))
    }
  }

  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(5)
      .withMaxSize(5)
}
