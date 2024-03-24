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

package org.typelevel.otel4s.sdk.metrics

import cats.Hash
import cats.Show

sealed trait InstrumentType extends Product with Serializable

object InstrumentType {

  private[metrics] sealed trait Synchronous extends InstrumentType
  private[metrics] sealed trait Asynchronous extends InstrumentType

  case object Counter extends Synchronous
  case object UpDownCounter extends Synchronous
  case object Histogram extends Synchronous
  case object ObservableCounter extends Asynchronous
  case object ObservableUpDownCounter extends Asynchronous
  case object ObservableGauge extends Asynchronous

  val values: Set[InstrumentType] = Set(
    Counter,
    UpDownCounter,
    Histogram,
    ObservableCounter,
    ObservableUpDownCounter,
    ObservableGauge
  )

  implicit val instrumentTypeHash: Hash[InstrumentType] =
    Hash.fromUniversalHashCode

  implicit val instrumentTypeShow: Show[InstrumentType] =
    Show.fromToString
}
