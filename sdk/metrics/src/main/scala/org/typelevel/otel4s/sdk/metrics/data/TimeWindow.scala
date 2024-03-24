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

package org.typelevel.otel4s.sdk.metrics.data

import scala.concurrent.duration.FiniteDuration

sealed trait TimeWindow {

  /** The start of the time window.
    */
  def start: FiniteDuration

  /** The end of the time window.
    */
  def end: FiniteDuration
}

object TimeWindow {

  def apply(start: FiniteDuration, end: FiniteDuration): TimeWindow =
    Impl(start, end)

  private final case class Impl(
      start: FiniteDuration,
      end: FiniteDuration
  ) extends TimeWindow
}
