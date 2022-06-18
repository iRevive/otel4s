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

package org.typelevel.otel4s.testkit

import java.{lang => jl, util => ju}

import cats.effect.Sync
import io.opentelemetry.sdk.OpenTelemetrySdk
import io.opentelemetry.sdk.metrics.data.{
  DoublePointData,
  LongPointData,
  MetricDataType,
  Data => JData,
  HistogramPointData => JHistogramPointData,
  MetricData => JMetricData,
  PointData => JPointData,
  SummaryPointData => JSummaryPointData
}
import io.opentelemetry.api.common.{Attributes => JAttributes}
import io.opentelemetry.api.common.{AttributeType => JAttributeType}
import io.opentelemetry.sdk.metrics.{SdkMeterProvider, SdkMeterProviderBuilder}
import io.opentelemetry.sdk.testing.exporter.InMemoryMetricReader
import org.typelevel.otel4s.{Attribute, AttributeKey}

import scala.jdk.CollectionConverters._

trait Sdk[F[_]] {
  def sdk: OpenTelemetrySdk
  def metrics: F[List[Metric]]
}

object Sdk {

  def create[F[_]: Sync](
      customize: SdkMeterProviderBuilder => SdkMeterProviderBuilder = identity
  ): Sdk[F] = {
    val metricReader = InMemoryMetricReader.create()

    val meterProviderBuilder = SdkMeterProvider
      .builder()
      .registerMetricReader(metricReader)

    val meterProvider = customize(meterProviderBuilder).build()

    val openTelemetrySdk = OpenTelemetrySdk
      .builder()
      .setMeterProvider(meterProvider)
      .build()

    new Sdk[F] {
      val sdk: OpenTelemetrySdk = openTelemetrySdk

      def metrics: F[List[Metric]] =
        Sync[F].delay {
          metricReader.collectAllMetrics().asScala.toList.map(makeMetric)
        }
    }
  }

  private def makeMetric(md: JMetricData): Metric = {

    def summaryPoint(data: JSummaryPointData): SummaryPointData =
      SummaryPointData(
        sum = data.getSum,
        count = data.getCount,
        values = data.getValues.asScala.toList.map(v =>
          QuantileData(v.getQuantile, v.getValue)
        )
      )

    def histogramPoint(data: JHistogramPointData): HistogramPointData =
      HistogramPointData(
        sum = data.getSum,
        count = data.getCount,
        boundaries = data.getBoundaries.asScala.toList.map(_.doubleValue()),
        counts = data.getCounts.asScala.toList.map(_.longValue())
      )

    def pointData[A <: JPointData, B](point: A, f: A => B) =
      PointData(
        point.getStartEpochNanos,
        point.getEpochNanos,
        collectAttributes(point.getAttributes),
        f(point)
      )

    def collectDataPoints[A <: JPointData, B](data: JData[A], f: A => B) =
      data.getPoints.asScala.toList
        .map(point => pointData[A, B](point, f))

    val data = md.getType match {
      case MetricDataType.LONG_GAUGE =>
        MetricData.LongGauge(
          collectDataPoints[LongPointData, Long](
            md.getLongGaugeData,
            _.getValue
          )
        )

      case MetricDataType.DOUBLE_GAUGE =>
        MetricData.DoubleGauge(
          collectDataPoints[DoublePointData, Double](
            md.getDoubleGaugeData,
            _.getValue
          )
        )

      case MetricDataType.LONG_SUM =>
        MetricData.LongSum(
          collectDataPoints[LongPointData, Long](
            md.getLongSumData,
            _.getValue
          )
        )

      case MetricDataType.DOUBLE_SUM =>
        MetricData.DoubleSum(
          collectDataPoints[DoublePointData, Double](
            md.getDoubleSumData,
            _.getValue
          )
        )

      case MetricDataType.SUMMARY =>
        MetricData.Summary(
          collectDataPoints[JSummaryPointData, SummaryPointData](
            md.getSummaryData,
            summaryPoint
          )
        )

      case MetricDataType.HISTOGRAM =>
        MetricData.Histogram(
          collectDataPoints[JHistogramPointData, HistogramPointData](
            md.getHistogramData,
            histogramPoint
          )
        )

      case MetricDataType.EXPONENTIAL_HISTOGRAM =>
        MetricData.ExponentialHistogram(
          collectDataPoints[JHistogramPointData, HistogramPointData](
            md.getHistogramData,
            histogramPoint
          )
        )
    }

    val scope = md.getInstrumentationScopeInfo
    val resource = md.getResource

    Metric(
      name = md.getName,
      description = Option(md.getDescription),
      unit = Option(md.getUnit),
      scope = InstrumentationScope(
        name = scope.getName,
        version = Option(scope.getVersion),
        schemaUrl = Option(scope.getSchemaUrl)
      ),
      resource = MetricResource(
        schemaUrl = Option(resource.getSchemaUrl),
        attributes = collectAttributes(resource.getAttributes)
      ),
      data = data
    )
  }

  private[testkit] def collectAttributes(
      attributes: JAttributes
  ): List[Attribute[_]] =
    attributes.asMap().asScala.toList.collect {
      case (attribute, value: String)
          if attribute.getType == JAttributeType.STRING =>
        Attribute(AttributeKey.string(attribute.getKey), value)

      case (attribute, value: jl.Boolean)
          if attribute.getType == JAttributeType.BOOLEAN =>
        Attribute(AttributeKey.boolean(attribute.getKey), value.booleanValue())

      case (attribute, value: jl.Long)
          if attribute.getType == JAttributeType.LONG =>
        Attribute(AttributeKey.long(attribute.getKey), value.longValue())

      case (attribute, value: jl.Double)
          if attribute.getType == JAttributeType.DOUBLE =>
        Attribute(AttributeKey.double(attribute.getKey), value.doubleValue())

      case (attribute, value: ju.List[String] @unchecked)
          if attribute.getType == JAttributeType.STRING_ARRAY =>
        Attribute(
          AttributeKey.stringList(attribute.getKey),
          value.asScala.toList
        )

      case (attribute, value: ju.List[jl.Boolean] @unchecked)
          if attribute.getType == JAttributeType.BOOLEAN_ARRAY =>
        Attribute(
          AttributeKey.booleanList(attribute.getKey),
          value.asScala.toList.map(_.booleanValue())
        )

      case (attribute, value: ju.List[jl.Long] @unchecked)
          if attribute.getType == JAttributeType.LONG_ARRAY =>
        Attribute(
          AttributeKey.longList(attribute.getKey),
          value.asScala.toList.map(_.longValue())
        )

      case (attribute, value: ju.List[jl.Double] @unchecked)
          if attribute.getType == JAttributeType.DOUBLE_ARRAY =>
        Attribute(
          AttributeKey.doubleList(attribute.getKey),
          value.asScala.toList.map(_.doubleValue())
        )
    }

}
