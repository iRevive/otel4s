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

import cats.Show
import cats.syntax.foldable._
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.sdk.metrics.Aggregation

/** A view configures how measurements are aggregated and exported as metrics.
  */
sealed trait View {

  /** The custom name of the resulting metric.
    *
    * `None` means the name of the matched instrument should be used.
    */
  def name: Option[String]

  /** The custom description of the resulting metric.
    *
    * `None` means the description of the matched instrument should be used.
    */
  def description: Option[String]

  /** The [[Aggregation]] of the resulting metric.
    */
  def aggregation: Aggregation

  /** The [[AttributesProcessor]] associated with this view.
    */
  def attributesProcessor: AttributesProcessor

  /** The cardinality limit of this view - the maximum number of series for a
    * metric.
    */
  def cardinalityLimit: Int

  override final def toString: String =
    Show[View].show(this)
}

object View {

  /** Builder of a [[View]].
    */
  sealed trait Builder {

    /** Sets the custom name of the resulting metric.
      *
      * @param name
      *   the name to use
      */
    def withName(name: String): Builder

    /** Sets the custom description of the resulting metric.
      *
      * @param description
      *   the description to use
      */
    def withDescription(description: String): Builder

    /** Sets the aggregation to use with the resulting metric.
      *
      * @param aggregation
      *   the aggregation to use
      */
    def withAggregation(aggregation: Aggregation): Builder

    /** Sets the cardinality limit - the maximum number of series for a metric.
      *
      * @param limit
      *   the limit to use
      */
    def withCardinalityLimit(limit: Int): Builder

    /** Adds an attribute filter which retains keys included in the `retain`.
      *
      * @param retain
      *   the key to retain
      */
    def addAttributeFilter(retain: Set[String]): Builder

    /** Adds an attribute filter which retains attributes that satisfy the
      * `filter`.
      *
      * @param filter
      *   the filter to use
      */
    def addAttributeFilter(filter: Attribute[_] => Boolean): Builder

    /** Creates a [[View]] using the configuration of this builder.
      */
    def build: View
  }

  /** * Returns an empty [[Builder]] of a [[View]].
    */
  def builder: Builder =
    BuilderImpl()

  implicit val viewShow: Show[View] = Show.show { view =>
    val name = view.name.foldMap(n => s"name=$n, ")
    val description = view.description.foldMap(d => s"description=$d, ")

    s"View{$name${description}aggregation=${view.aggregation}, cardinalityLimit=${view.cardinalityLimit}}"
  }

  private final case class Impl(
      name: Option[String],
      description: Option[String],
      aggregation: Aggregation,
      attributesProcessor: AttributesProcessor,
      cardinalityLimit: Int
  ) extends View

  private final case class BuilderImpl(
      name: Option[String] = None,
      description: Option[String] = None,
      aggregation: Option[Aggregation] = None,
      cardinalityLimit: Option[Int] = None,
      attributesProcessors: List[AttributesProcessor] = Nil
  ) extends Builder {

    def withName(name: String): Builder = copy(name = Some(name))

    def withDescription(description: String): Builder =
      copy(description = Some(description))

    def withAggregation(aggregation: Aggregation): Builder =
      copy(aggregation = Some(aggregation))

    def addAttributeFilter(retain: Set[String]): Builder =
      copy(attributesProcessors =
        attributesProcessors :+ AttributesProcessor.retain(retain)
      )

    def addAttributeFilter(filter: Attribute[_] => Boolean): Builder =
      copy(attributesProcessors =
        attributesProcessors :+ AttributesProcessor.attributePredicate(filter)
      )

    def withCardinalityLimit(limit: Int): Builder =
      copy(cardinalityLimit = Some(limit))

    def build: View =
      Impl(
        name,
        description,
        aggregation.getOrElse(Aggregation.default),
        attributesProcessors.combineAll,
        cardinalityLimit.getOrElse(2000)
      )
  }

}
