package org.typelevel.otel4s.sdk.metrics.internal.aggregation

import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.metrics.data.PointData
import org.typelevel.otel4s.sdk.metrics.internal.Measurement

private[aggregation] object Utils {

  def measurementToNumberPoint[
      A: MeasurementValue
  ]: Measurement[A] => PointData.NumberPoint =
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
