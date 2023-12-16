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

package org.typelevel.otel4s.sdk.trace
package samplers

import munit._
import org.scalacheck.Gen
import org.scalacheck.Prop

class ParentBasedSamplerSuite extends ScalaCheckSuite {

  private val samplerGen: Gen[Sampler] =
    Gen.oneOf(Sampler.AlwaysOn, Sampler.AlwaysOff)

  private val samplerChoiceGen: Gen[SamplerChoice] =
    Gen.oneOf(SamplerChoice.AlwaysOn, SamplerChoice.AlwaysOff)

  test("created with root sampler - has correct description and toString") {
    Prop.forAll(samplerGen) { root =>
      val sampler = Sampler.parentBased(root)

      val expected =
        s"ParentBased{root=$root, " +
          "remoteParentSampled=AlwaysOnSampler, " +
          "remoteParentNotSampled=AlwaysOffSampler, " +
          "localParentSampled=AlwaysOnSampler, " +
          "localParentNotSampled=AlwaysOffSampler}"

      assertEquals(sampler.description, expected)
      assertEquals(sampler.toString, expected)
    }
  }

  test("created with root sampler - returns correct sampling result") {
    Prop.forAll(samplerChoiceGen, ShouldSampleInput.shouldSampleInputGen) {
      (choice, input) =>
        val sampler = Sampler.parentBased(choice.sampler)

        val expected = input.parentContext match {
          // valid remote parent
          case Some(parent) if parent.isValid && parent.isRemote =>
            if (parent.isSampled) SamplingResult.RecordAndSample
            else SamplingResult.Drop

          // valid local parent
          case Some(parent) if parent.isValid =>
            if (parent.isSampled) SamplingResult.RecordAndSample
            else SamplingResult.Drop

          case _ =>
            choice.result
        }

        val result = sampler.shouldSample(
          input.parentContext,
          input.traceId,
          input.name,
          input.spanKind,
          input.attributes,
          input.parentLinks
        )

        assertEquals(result, expected)
    }
  }

  test("created with all samplers - has correct description and toString") {
    Prop.forAll(samplerGen, samplerGen, samplerGen, samplerGen, samplerGen) {
      (
          root,
          remoteParentSampled,
          remoteParentNotSampled,
          localParentSampled,
          localParentNotSampled
      ) =>
        val sampler = Sampler
          .parentBasedBuilder(root)
          .withRemoteParentSampled(remoteParentSampled)
          .withRemoteParentNotSampled(remoteParentNotSampled)
          .withLocalParentSampled(localParentSampled)
          .withLocalParentNotSampled(localParentNotSampled)
          .build

        val expected =
          s"ParentBased{root=$root, " +
            s"remoteParentSampled=$remoteParentSampled, " +
            s"remoteParentNotSampled=$remoteParentNotSampled, " +
            s"localParentSampled=$localParentSampled, " +
            s"localParentNotSampled=$localParentNotSampled}"

        assertEquals(sampler.description, expected)
        assertEquals(sampler.toString, expected)
    }
  }

  test("created with all samplers - returns correct sampling result") {
    Prop.forAll(
      samplerChoiceGen,
      samplerChoiceGen,
      samplerChoiceGen,
      samplerChoiceGen,
      samplerChoiceGen,
      ShouldSampleInput.shouldSampleInputGen
    ) {
      (
          root,
          remoteParentSampled,
          remoteParentNotSampled,
          localParentSampled,
          localParentNotSampled,
          input
      ) =>
        val sampler = Sampler
          .parentBasedBuilder(root.sampler)
          .withRemoteParentSampled(remoteParentSampled.sampler)
          .withRemoteParentNotSampled(remoteParentNotSampled.sampler)
          .withLocalParentSampled(localParentSampled.sampler)
          .withLocalParentNotSampled(localParentNotSampled.sampler)
          .build

        val expected = input.parentContext match {
          // valid remote parent
          case Some(parent) if parent.isValid && parent.isRemote =>
            if (parent.isSampled) remoteParentSampled.result
            else remoteParentNotSampled.result

          // valid local parent
          case Some(parent) if parent.isValid =>
            if (parent.isSampled) localParentSampled.result
            else localParentNotSampled.result

          case _ =>
            root.result
        }

        val result = sampler.shouldSample(
          input.parentContext,
          input.traceId,
          input.name,
          input.spanKind,
          input.attributes,
          input.parentLinks
        )

        assertEquals(result, expected)
    }
  }

  sealed abstract class SamplerChoice(
      val sampler: Sampler,
      val result: SamplingResult
  )

  object SamplerChoice {
    case object AlwaysOn
        extends SamplerChoice(Sampler.AlwaysOn, SamplingResult.RecordAndSample)

    case object AlwaysOff
        extends SamplerChoice(Sampler.AlwaysOff, SamplingResult.Drop)
  }
}
