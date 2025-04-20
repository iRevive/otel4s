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

package org.typelevel.otel4s

import cats.Show
import cats.kernel.laws.discipline.HashTests
import munit.DisciplineSuite
import org.scalacheck.Prop
import org.typelevel.otel4s.scalacheck.{Arbitraries, Cogens}

class ValueSuite extends DisciplineSuite {
  import Arbitraries.valueArbitrary
  import Cogens.valueCogen

  // Test Hash laws
  checkAll("Value.HashLaws", HashTests[Value].hash)

  // Test Show instance
  test("Show[Value]") {
    Prop.forAll(valueArbitrary.arbitrary) { value =>
      def render(v: Value): String = v match {
        case Value.StringValue(value)    => s"StringValue($value)"
        case Value.BooleanValue(value)   => s"BooleanValue($value)"
        case Value.LongValue(value)      => s"LongValue($value)"
        case Value.DoubleValue(value)    => s"DoubleValue($value)"
        case Value.ByteArrayValue(value) => s"ByteArrayValue(${java.util.Arrays.toString(value)})"
        case Value.ArrayValue(values)    => s"ArrayValue(${values.map(render).mkString("[", ", ", "]")})"
        case Value.MapValue(values) =>
          s"MapValue(${values.map { case (k, v) => s"$k -> ${render(v)}" }.mkString("{", ", ", "}")})"
      }

      val expected = render(value)

      assertEquals(Show[Value].show(value), expected)
      assertEquals(value.toString, expected)
    }
  }
}
