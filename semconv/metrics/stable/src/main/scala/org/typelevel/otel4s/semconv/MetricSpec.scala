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

package org.typelevel.otel4s.semconv

/** The metric specification.
  */
sealed trait MetricSpec {

  /** The name of the metric.
    */
  def name: String

  /** The description of the metric.
    */
  def description: String

  /** The measurement unit of the metric.
    */
  def unit: String

  /** The stability of the metric.
    */
  def stability: Stability

  /** The stability of the attribute.
    */
  def attributeSpecs: List[AttributeSpec[_]]

}

object MetricSpec {
  private[semconv] trait Unsealed extends MetricSpec
}
