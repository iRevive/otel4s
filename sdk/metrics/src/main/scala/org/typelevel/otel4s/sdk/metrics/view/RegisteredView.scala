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

package org.typelevel.otel4s.sdk.metrics.view

private[metrics] sealed trait RegisteredView {
  def selector: InstrumentSelector
  def view: View
}

private[metrics] object RegisteredView {

  def apply(selector: InstrumentSelector, view: View): RegisteredView =
    Impl(selector, view)

  private final case class Impl(
      selector: InstrumentSelector,
      view: View
  ) extends RegisteredView

}
