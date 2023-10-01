/*
 * Copyright 2023 Typelevel
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

package org.typelevel.otel4s.sdk
package trace
package samplers

import org.typelevel.otel4s.sdk.trace.data.LinkData
import org.typelevel.otel4s.trace.SpanContext
import org.typelevel.otel4s.trace.SpanKind

trait Sampler {
  def shouldSample(
      parentContext: Option[SpanContext],
      traceId: String,
      name: String,
      kind: SpanKind,
      attributes: Attributes,
      parentLinks: List[LinkData]
  ): SamplingResult

  def description: String
}

object Sampler {

  def recordAndSample: Sampler =
    new Sampler {
      def shouldSample(
          parentContext: Option[SpanContext],
          traceId: String,
          name: String,
          kind: SpanKind,
          attributes: Attributes,
          parentLinks: List[LinkData]
      ): SamplingResult = SamplingResult.recordAndSample

      def description: String = "record and sample"
    }

}