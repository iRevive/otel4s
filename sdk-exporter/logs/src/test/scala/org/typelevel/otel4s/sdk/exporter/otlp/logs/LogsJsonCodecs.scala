package org.typelevel.otel4s.sdk.exporter.otlp.logs

import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.typelevel.otel4s.Value
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
          "attributes" := log.attributes,
          //"droppedAttributesCount" := 0,
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
}
