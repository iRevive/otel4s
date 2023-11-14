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

import org.scalacheck.Arbitrary
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.sdk.trace.samplers.SamplingDecision

object Arbitraries {

  implicit val attributeArbitrary: Arbitrary[Attribute[_]] =
    Arbitrary(Gens.attribute)

  implicit val attributesArbitrary: Arbitrary[Attributes] =
    Arbitrary(Gens.attributes)

  implicit val resourceArbitrary: Arbitrary[Resource] =
    Arbitrary(Gens.resource)

  implicit val samplingDecisionArbitrary: Arbitrary[SamplingDecision] =
    Arbitrary(Gens.samplingDecision)

}
