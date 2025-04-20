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

import cats.kernel.laws.discipline.HashTests
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import scala.collection.immutable

class ValueSuite extends DisciplineSuite {

  // Generate arbitrary Value instances for testing
  implicit val arbValue: Arbitrary[Value] = Arbitrary {
    val genString = Gen.alphaNumStr.map(Value.string)
    val genBoolean = Gen.oneOf(true, false).map(Value.boolean)
    val genLong = Gen.long.map(Value.long)
    val genDouble = Gen.double.map(Value.double)
    val genByteArray = Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(_.toArray).map(Value.bytes)

    // For recursive types (array and map), we need to limit the depth to avoid stack overflow
    def genValueWithDepth(depth: Int): Gen[Value] = {
      if (depth <= 0) {
        // Base case: only generate non-recursive values
        Gen.oneOf(genString, genBoolean, genLong, genDouble, genByteArray)
      } else {
        // Recursive case: generate any value type, including arrays and maps
        val genArray = Gen.listOf(genValueWithDepth(depth - 1)).map(_.toList).map(Value.array)
        val genMap = for {
          keys <- Gen.listOf(Gen.alphaNumStr)
          values <- Gen.listOf(genValueWithDepth(depth - 1))
          pairs = keys.zip(values).toMap
        } yield Value.map(pairs)
        Gen.oneOf(genString, genBoolean, genLong, genDouble, genByteArray, genArray, genMap)
      }
    }

    genValueWithDepth(2) // Limit recursion depth to 2 to avoid stack overflow
  }

  // Generate arbitrary functions from Value to Value
  implicit val arbValueFunction: Arbitrary[Value => Value] = Arbitrary {
    Gen.oneOf(
      Gen.const((v: Value) => v), // identity function
      Gen.const((v: Value) => Value.string("constant")), // constant function
      Gen.const((v: Value) => Value.array(List(v))), // wrap in array
      arbitrary[Value].map(constant => (_: Value) => constant) // constant function with random value
    )
  }

  // Test Hash laws
  checkAll("Value.HashLaws", HashTests[Value].hash)

  // Test Show instance
  test("Show[Value]") {
    // Test simple values
    assertEquals(Value.string("test").toString, "StringValue(test)")
    assertEquals(Value.boolean(true).toString, "BooleanValue(true)")
    assertEquals(Value.long(123L).toString, "LongValue(123)")
    assertEquals(Value.double(3.14).toString, "DoubleValue(3.14)")

    // Test array value
    val arrayValue = Value.array(List(Value.string("a"), Value.long(1)))
    assertEquals(arrayValue.toString, "ArrayValue([StringValue(a), LongValue(1)])")

    // Test map value
    val mapValue = Value.map(Map("key" -> Value.boolean(true)))
    assertEquals(mapValue.toString, "MapValue({key -> BooleanValue(true)})")
  }
}
