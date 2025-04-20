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

package org.typelevel.otel4s.logs

import cats.Show
import cats.kernel.laws.discipline.HashTests
import cats.syntax.show._
import munit.DisciplineSuite
import munit.internal.PlatformCompat
import org.scalacheck.Prop
import org.scalacheck.Test
import org.typelevel.otel4s.logs.scalacheck.Arbitraries
import org.typelevel.otel4s.logs.scalacheck.Cogens
import org.typelevel.otel4s.logs.scalacheck.Gens

class SeveritySuite extends DisciplineSuite {
  import Cogens.severityCogen
  import Arbitraries.severityArbitrary

  checkAll("Severity.HashLaws", HashTests[Severity].hash)

  test("Show[Severity]") {
    Prop.forAll(Gens.severity) { severity =>
      val expected = severity.toString
      assertEquals(Show[Severity].show(severity), expected)
    }
  }

  test("fromNumber returns the correct Severity") {
    Prop.forAll(Gens.severity) { severity =>
      assertEquals(Severity.fromNumber(severity.severity), Some(severity))
    }
  }

  test("fromNumber returns None for invalid numbers") {
    assertEquals(Severity.fromNumber(-1), None)
    assertEquals(Severity.fromNumber(25), None)
  }

  test("Severity values match the OpenTelemetry specification") {
    assertEquals(Severity.unspecified.severity, 0)
    assertEquals(Severity.trace.number, 1)
    assertEquals(Severity.trace2.number, 2)
    assertEquals(Severity.trace3.number, 3)
    assertEquals(Severity.trace4.number, 4)
    assertEquals(Severity.debug.severity, 5)
    assertEquals(Severity.debug2.severity, 6)
    assertEquals(Severity.debug3.severity, 7)
    assertEquals(Severity.debug4.severity, 8)
    assertEquals(Severity.info.severity, 9)
    assertEquals(Severity.info2.severity, 10)
    assertEquals(Severity.info3.severity, 11)
    assertEquals(Severity.info4.severity, 12)
    assertEquals(Severity.warn.severity, 13)
    assertEquals(Severity.warn2.severity, 14)
    assertEquals(Severity.warn3.severity, 15)
    assertEquals(Severity.warn4.severity, 16)
    assertEquals(Severity.error.severity, 17)
    assertEquals(Severity.error2.severity, 18)
    assertEquals(Severity.error3.severity, 19)
    assertEquals(Severity.error4.severity, 20)
    assertEquals(Severity.fatal.severity, 21)
    assertEquals(Severity.fatal2.severity, 22)
    assertEquals(Severity.fatal3.severity, 23)
    assertEquals(Severity.fatal4.severity, 24)
  }

}
