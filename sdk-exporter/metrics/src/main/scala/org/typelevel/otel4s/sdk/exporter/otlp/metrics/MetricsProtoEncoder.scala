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

package org.typelevel.otel4s
package sdk
package exporter.otlp.metrics

import com.google.protobuf.ByteString
import io.opentelemetry.proto.collector.metrics.v1.metrics_service.ExportMetricsServiceRequest
import io.opentelemetry.proto.metrics.v1.{metrics => Proto}
import io.opentelemetry.proto.metrics.v1.metrics.ResourceMetrics
import io.opentelemetry.proto.metrics.v1.metrics.ScopeMetrics
import org.typelevel.otel4s.sdk.exporter.otlp.ProtoEncoder
import org.typelevel.otel4s.sdk.metrics.data.AggregationTemporality
import org.typelevel.otel4s.sdk.metrics.data.ExemplarData
import org.typelevel.otel4s.sdk.metrics.data.MetricData
import org.typelevel.otel4s.sdk.metrics.data.MetricPoints
import org.typelevel.otel4s.sdk.metrics.data.PointData
import scalapb_circe.Printer

/** @see
  *   [[https://github.com/open-telemetry/opentelemetry-proto/blob/v1.0.0/opentelemetry/proto/common/v1/common.proto]]
  */
private object MetricsProtoEncoder {
  implicit val jsonPrinter: Printer = new ProtoEncoder.JsonPrinter

  implicit val aggregationTemporalityEncoder: ProtoEncoder[
    AggregationTemporality,
    Proto.AggregationTemporality
  ] = {
    case AggregationTemporality.Delta =>
      Proto.AggregationTemporality.AGGREGATION_TEMPORALITY_DELTA
    case AggregationTemporality.Cumulative =>
      Proto.AggregationTemporality.AGGREGATION_TEMPORALITY_CUMULATIVE
  }

  implicit val exemplarEncoder: ProtoEncoder[
    ExemplarData,
    Proto.Exemplar
  ] = { exemplar =>
    val value = exemplar match {
      case e: ExemplarData.LongExemplar =>
        Proto.Exemplar.Value.AsInt(e.value)
      case e: ExemplarData.DoubleExemplar =>
        Proto.Exemplar.Value.AsDouble(e.value)
    }

    val traceId =
      exemplar.traceContext
        .map(v => ByteString.copyFrom(v.traceId.toArray))
        .getOrElse(ByteString.EMPTY)

    val spanId =
      exemplar.traceContext
        .map(v => ByteString.copyFrom(v.spanId.toArray))
        .getOrElse(ByteString.EMPTY)

    Proto.Exemplar(
      filteredAttributes = ProtoEncoder.encode(exemplar.filteredAttributes),
      timeUnixNano = exemplar.timestamp.toNanos,
      value = value,
      spanId = spanId,
      traceId = traceId
    )
  }

  implicit val numberPointEncoder: ProtoEncoder[
    PointData.NumberPoint,
    Proto.NumberDataPoint
  ] = { point =>
    val value = point match {
      case long: PointData.LongNumber =>
        Proto.NumberDataPoint.Value.AsInt(long.value)
      case double: PointData.DoubleNumber =>
        Proto.NumberDataPoint.Value.AsDouble(double.value)
    }

    Proto.NumberDataPoint(
      ProtoEncoder.encode(point.attributes),
      point.timeWindow.start.toNanos,
      point.timeWindow.end.toNanos,
      value = value,
      exemplars = point.exemplars.map(ProtoEncoder.encode(_)),
    )
  }

  implicit val dataEncoder: ProtoEncoder[MetricPoints, Proto.Metric.Data] = {
    case sum: MetricPoints.Sum =>
      Proto.Metric.Data.Sum(
        Proto.Sum(
          sum.points.map(ProtoEncoder.encode(_)),
          ProtoEncoder.encode(sum.aggregationTemporality),
          sum.monotonic
        )
      )

    case gauge: MetricPoints.Gauge =>
      Proto.Metric.Data.Gauge(
        Proto.Gauge(gauge.points.map(ProtoEncoder.encode(_)))
      )

    case summary: MetricPoints.Summary =>
      Proto.Metric.Data.Summary(
        Proto.Summary(
          summary.points.map(p =>
            Proto.SummaryDataPoint(
              ProtoEncoder.encode(p.attributes),
              p.timeWindow.start.toNanos,
              p.timeWindow.end.toNanos,
              p.count,
              p.sum,
              p.percentileValues.map { q =>
                Proto.SummaryDataPoint
                  .ValueAtQuantile(q.quantile, q.value)
              }
            )
          )
        )
      )

    case histogram: MetricPoints.Histogram =>
      Proto.Metric.Data.Histogram(
        Proto.Histogram(
          histogram.points.map(p =>
            Proto.HistogramDataPoint(
              attributes = ProtoEncoder.encode(p.attributes),
              startTimeUnixNano = p.timeWindow.start.toNanos,
              timeUnixNano = p.timeWindow.end.toNanos,
              count = p.stats.map(_.count).getOrElse(0L),
              sum = p.stats.map(_.sum),
              bucketCounts = p.counts,
              explicitBounds = p.boundaries.boundaries,
              exemplars = p.exemplars.map(ProtoEncoder.encode(_)),
              min = p.stats.map(_.min),
              max = p.stats.map(_.max)
            )
          ),
          ProtoEncoder.encode(histogram.aggregationTemporality)
        )
      )

    case exponentialHistogram: MetricPoints.ExponentialHistogram =>
      Proto.Metric.Data.ExponentialHistogram(
        Proto.ExponentialHistogram(
          exponentialHistogram.points.map(p =>
            Proto
              .ExponentialHistogramDataPoint(
                attributes = ProtoEncoder.encode(p.attributes),
                startTimeUnixNano = p.timeWindow.start.toNanos,
                timeUnixNano = p.timeWindow.end.toNanos,
                count = p.count,
                sum = Some(p.sum),
                scale = 0, // todo scale
                zeroCount = p.zeroCount,
                positive = Some(
                  Proto.ExponentialHistogramDataPoint.Buckets(
                    p.positiveBuckets.offset,
                    p.positiveBuckets.bucketCounts
                  )
                ),
                negative = Some(
                  Proto.ExponentialHistogramDataPoint.Buckets(
                    p.negativeBuckets.offset,
                    p.negativeBuckets.bucketCounts
                  )
                ),
                exemplars = p.exemplars.map(ProtoEncoder.encode(_)),
                min = p.min,
                max = p.max,
                // zeroThreshold = , // todo?
              )
          ),
          ProtoEncoder.encode(exponentialHistogram.aggregationTemporality)
        )
      )
  }

  implicit val metricDataEncoder: ProtoEncoder[
    MetricData,
    Proto.Metric
  ] = { metric =>
    Proto.Metric(
      metric.name,
      metric.description.getOrElse(""),
      unit = metric.unit.getOrElse(""),
      data = ProtoEncoder.encode(metric.data)
    )
  }

  implicit val exportMetricsRequest: ProtoEncoder[
    List[MetricData],
    ExportMetricsServiceRequest
  ] = { metrics =>
    val resourceSpans =
      metrics
        .groupBy(_.resource)
        .map { case (resource, resourceSpans) =>
          val scopeSpans: List[ScopeMetrics] =
            resourceSpans
              .groupBy(_.instrumentationScope)
              .map { case (scope, spans) =>
                ScopeMetrics(
                  scope = Some(ProtoEncoder.encode(scope)),
                  metrics = spans.map(metric => ProtoEncoder.encode(metric)),
                  schemaUrl = scope.schemaUrl.getOrElse("")
                )
              }
              .toList

          ResourceMetrics(
            Some(ProtoEncoder.encode(resource)),
            scopeSpans,
            resource.schemaUrl.getOrElse("")
          )
        }
        .toList

    ExportMetricsServiceRequest(resourceSpans)
  }

}
