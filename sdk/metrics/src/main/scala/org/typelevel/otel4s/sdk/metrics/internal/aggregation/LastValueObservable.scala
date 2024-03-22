package org.typelevel.otel4s.sdk.metrics.internal
package aggregation

import cats.Applicative
import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.metrics.data.{AggregationTemporality, Data, MetricData, PointData}

final class LastValueObservable[
    F[_]: Applicative,
    A: MeasurementValue
] extends Aggregator.Observable[F, A] {

  def diff(previous: Measurement[A], current: Measurement[A]): Measurement[A] =
    current

  def toMetricData(
      measurements: Vector[Measurement[A]],
      resource: TelemetryResource,
      scope: InstrumentationScope,
      descriptor: MetricDescriptor,
      temporality: AggregationTemporality
  ): F[MetricData] = {
    val points = measurements.map(measurement => toPointData(measurement))

    Applicative[F].pure(
      MetricData(
        resource,
        scope,
        descriptor.name,
        descriptor.description,
        descriptor.sourceInstrument.unit,
        Data.Gauge(points)
      )
    )
  }

  private val toPointData: Measurement[A] => PointData.NumberPoint =
    MeasurementValue[A] match {
      case MeasurementValue.LongMeasurementValue(cast) =>
        m =>
          PointData.LongNumber(
            m.startTimestamp,
            m.collectTimestamp,
            m.attributes,
            Vector.empty,
            cast(m.value)
          )

      case MeasurementValue.DoubleMeasurementValue(cast) =>
        m =>
          PointData.DoubleNumber(
            m.startTimestamp,
            m.collectTimestamp,
            m.attributes,
            Vector.empty,
            cast(m.value)
          )
    }

}

object LastValueObservable {

  def apply[F[_]: Applicative, A: MeasurementValue]: LastValueObservable[F, A] =
    new LastValueObservable[F, A]

}
