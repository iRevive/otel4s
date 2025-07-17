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

package org.typelevel.otel4s.sdk.exporter.otlp.logs

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import org.typelevel.otel4s.AnyValue
import org.typelevel.otel4s.sdk.exporter.otlp.JsonCodecs
import org.typelevel.otel4s.sdk.logs.data.LogRecordData
import scodec.bits.ByteVector

// the instances mimic Protobuf encoding
private object LogsJsonCodecs extends JsonCodecs {

  implicit val logRecordDataJsonEncoder: Encoder[LogRecordData] =
    Encoder.instance { log =>
      Json
        .obj(
          "timeUnixNano" := log.timestamp.map(_.toNanos.toString),
          "observedTimeUnixNano" := log.observedTimestamp.toNanos.toString,
          "severityNumber" := log.severity.map(_.severity),
          "severityText" := log.severityText,
          "body" := log.body.map(encodeValue),
          "attributes" := log.attributes.elements,
          "droppedAttributesCount" := log.attributes.dropped,
          "traceId" := log.traceContext.map(_.traceId.toHex),
          "spanId" := log.traceContext.map(_.spanId.toHex)
        )
        .dropNullValues
        .dropEmptyValues
    }

  implicit val logRecordDataListJsonEncoder: Encoder[List[LogRecordData]] =
    Encoder.instance { logs =>
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

      Json.obj("resourceLogs" := resourceLogs).dropEmptyValues
    }

  private def encodeValue(value: AnyValue): Json = {
    value match {
      case AnyValue.EmptyValueImpl        => Json.Null
      case AnyValue.StringValueImpl(v)    => Json.obj("stringValue" := v)
      case AnyValue.BooleanValueImpl(v)   => Json.obj("boolValue" := v)
      case AnyValue.LongValueImpl(v)      => Json.obj("intValue" := v.toString)
      case AnyValue.DoubleValueImpl(v)    => Json.obj("doubleValue" := v)
      case AnyValue.ByteArrayValueImpl(v) => Json.obj("bytesValue" := ByteVector(v).toBase64)
      case AnyValue.ListValueImpl(values) => Json.obj("arrayValue" := Json.obj("values" := values.map(encodeValue)))
      case AnyValue.MapValueImpl(values) =>
        Json.obj("kvlistValue" := Json.obj("values" := values.map { case (k, v) =>
          Json.obj("key" := k, "value" := encodeValue(v))
        }))
    }
  }
}
