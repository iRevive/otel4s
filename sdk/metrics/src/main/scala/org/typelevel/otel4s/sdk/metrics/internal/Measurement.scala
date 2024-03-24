/*
 * Copyright 2024 Typelevel
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

package org.typelevel.otel4s.sdk.metrics.internal

import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.metrics.data.TimeWindow

import scala.concurrent.duration.FiniteDuration

sealed trait Measurement[A] {
  def timeWindow: TimeWindow
  def attributes: Attributes
  def value: A

  def withAttributes(attributes: Attributes): Measurement[A]

  def withStartTimestamp(start: FiniteDuration): Measurement[A]

  def withValue(a: A): Measurement[A]
}

object Measurement {

  def apply[A](
      timeWindow: TimeWindow,
      attributes: Attributes,
      value: A
  ): Measurement[A] =
    Impl(timeWindow, attributes, value)

  private final case class Impl[A](
      timeWindow: TimeWindow,
      attributes: Attributes,
      value: A
  ) extends Measurement[A] {
    def withAttributes(attributes: Attributes): Measurement[A] =
      copy(attributes = attributes)

    def withStartTimestamp(start: FiniteDuration): Measurement[A] =
      copy(timeWindow = TimeWindow(start, timeWindow.end))

    def withValue(a: A): Measurement[A] =
      copy(value = a)
  }
  /*
  final case class LongMeasurement(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      value: Long
  ) extends Measurement[Long] {
    def withAttributes(attributes: Attributes): Measurement[Long] =
      copy(attributes = attributes)

    def withStartTimestamp(start: FiniteDuration): Measurement[Long] =
      copy(startTimestamp = start)
  }

  final case class DoubleMeasurement(
      startTimestamp: FiniteDuration,
      collectTimestamp: FiniteDuration,
      attributes: Attributes,
      value: Double
  ) extends Measurement[Double] {
    def withAttributes(attributes: Attributes): Measurement[Double] =
      copy(attributes = attributes)

    def withStartTimestamp(start: FiniteDuration): Measurement[Double] =
      copy(startTimestamp = start)
  }*/
}
