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

import cats.Semigroup
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.sdk.context.Context

/** The AttributesProcessor is used to define the actual set of attributes that
  * will be used in a metric.
  */
sealed trait AttributesProcessor {

  /** Processes a set of attributes, returning the desired set.
    *
    * @param attributes
    *   the attributes associated with an incoming measurement
    *
    * @param context
    *   the context associated with the measurement
    */
  def process(attributes: Attributes, context: Context): Attributes
}

object AttributesProcessor {

  def noop: AttributesProcessor =
    new AttributesProcessor {
      def process(attributes: Attributes, context: Context): Attributes =
        attributes
    }

  def filterByKeyName(keepWhen: String => Boolean): AttributesProcessor =
    new AttributesProcessor {
      def process(attributes: Attributes, context: Context): Attributes =
        attributes.filter(attribute => keepWhen(attribute.key.name))
    }

  implicit val attributesProcessorSemigroup: Semigroup[AttributesProcessor] =
    new Semigroup[AttributesProcessor] {
      def combine(
          x: AttributesProcessor,
          y: AttributesProcessor
      ): AttributesProcessor =
        new AttributesProcessor {
          def process(incoming: Attributes, context: Context): Attributes =
            x.process(y.process(incoming, context), context)
        }
    }
}
