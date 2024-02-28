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

import cats.Hash
import cats.Show
import org.typelevel.otel4s.metrics.MeasurementValue

sealed trait InstrumentValueType

object InstrumentValueType {
  case object LongValue extends InstrumentValueType
  case object DoubleValue extends InstrumentValueType

  def of[A: MeasurementValue]: InstrumentValueType =
    MeasurementValue[A] match {
      case MeasurementValue.LongMeasurementValue(_)   => LongValue
      case MeasurementValue.DoubleMeasurementValue(_) => DoubleValue
    }

  implicit val instrumentValueTypeHash: Hash[InstrumentValueType] =
    Hash.fromUniversalHashCode

  implicit val instrumentValueTypeShow: Show[InstrumentValueType] =
    Show.fromToString
}
