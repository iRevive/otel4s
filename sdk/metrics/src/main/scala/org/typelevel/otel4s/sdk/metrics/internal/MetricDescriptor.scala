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

import org.typelevel.otel4s.sdk.metrics.view.View

trait MetricDescriptor {

  def name: String
  def description: Option[String]
  def view: View
  def sourceInstrument: InstrumentDescriptor

}

object MetricDescriptor {
  def apply(
      view: View,
      instrumentDescriptor: InstrumentDescriptor
  ): MetricDescriptor =
    Impl(
      view.name.getOrElse(instrumentDescriptor.name.toString),
      view.description.orElse(instrumentDescriptor.description),
      view,
      instrumentDescriptor
    )

  private final case class Impl(
      name: String,
      description: Option[String],
      view: View,
      sourceInstrument: InstrumentDescriptor
  ) extends MetricDescriptor

}
