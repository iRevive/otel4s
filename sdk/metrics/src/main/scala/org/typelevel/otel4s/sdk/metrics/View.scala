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

import cats.syntax.foldable._
import org.typelevel.otel4s.sdk.metrics.internal.AttributesProcessor

sealed trait View {
  def name: Option[String]
  def description: Option[String]
  def aggregation: Aggregation
  def attributesProcessor: AttributesProcessor
  def cardinalityLimit: Int
}

object View {

  sealed trait Builder {
    def withName(name: String): Builder
    def withDescription(description: String): Builder
    def withAggregation(aggregation: Aggregation): Builder
    def withAttributeFilter(retain: Set[String]): Builder
    def withAttributeFilter(filter: String => Boolean): Builder
    def withCardinalityLimit(limit: Int): Builder
    def addAttributesProcessor(processor: AttributesProcessor): Builder
    def build: View
  }

  def builder: Builder =
    BuilderImpl()

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

    def withAttributeFilter(retain: Set[String]): Builder =
      copy(attributesProcessors =
        List(AttributesProcessor.filterByKeyName(retain.contains))
      )

    def withAttributeFilter(filter: String => Boolean): Builder =
      copy(attributesProcessors =
        List(AttributesProcessor.filterByKeyName(filter))
      )

    def withCardinalityLimit(limit: Int): Builder =
      copy(cardinalityLimit = Some(limit))

    def addAttributesProcessor(processor: AttributesProcessor): Builder =
      copy(attributesProcessors = attributesProcessors :+ processor)

    def build: View = {
      val attributesProcessor =
        attributesProcessors.combineAllOption.getOrElse(AttributesProcessor.noop)

      Impl(
        name,
        description,
        aggregation.getOrElse(Aggregation.default),
        attributesProcessor,
        cardinalityLimit.getOrElse(2000)
      )
    }
  }

}
