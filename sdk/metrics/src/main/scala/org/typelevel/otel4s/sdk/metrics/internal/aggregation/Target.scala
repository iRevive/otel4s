package org.typelevel.otel4s.sdk.metrics.internal.aggregation

import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.metrics.MeasurementValue
import org.typelevel.otel4s.sdk.metrics.data.ExemplarData
import org.typelevel.otel4s.sdk.metrics.data.PointData

import scala.concurrent.duration.FiniteDuration

private sealed trait Target[A] { self =>
  type Exemplar <: ExemplarData
  type Point <: PointData.NumberPoint

  def makeExemplar: ExemplarData.Make[A, Exemplar]

  def makePointData(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      exemplars: Vector[Exemplar],
      value: A
  ): Point

}

private object Target {

  def apply[A: MeasurementValue]: Target[A] =
    MeasurementValue[A] match {
      case MeasurementValue.LongMeasurementValue(cast) =>
        new Target[A] {
          type Exemplar = ExemplarData.LongExemplar
          type Point = PointData.LongNumber

          val makeExemplar: ExemplarData.Make[A, Exemplar] =
            ExemplarData.Make.makeLong

          def makePointData(
              startTimestamp: FiniteDuration,
              collectTimestamp: FiniteDuration,
              attributes: Attributes,
              exemplars: Vector[ExemplarData.LongExemplar],
              value: A
          ): PointData.LongNumber =
            PointData.LongNumber(
              startTimestamp,
              collectTimestamp,
              attributes,
              exemplars,
              cast(value)
            )

        }

      case MeasurementValue.DoubleMeasurementValue(cast) =>
        new Target[A] {
          type Exemplar = ExemplarData.DoubleExemplar
          type Point = PointData.DoubleNumber

          val makeExemplar: ExemplarData.Make[A, Exemplar] =
            ExemplarData.Make.makeDouble

          def makePointData(
              startTimestamp: FiniteDuration,
              collectTimestamp: FiniteDuration,
              attributes: Attributes,
              exemplars: Vector[ExemplarData.DoubleExemplar],
              value: A
          ): PointData.DoubleNumber =
            PointData.DoubleNumber(
              startTimestamp,
              collectTimestamp,
              attributes,
              exemplars,
              cast(value)
            )

        }

    }

}
