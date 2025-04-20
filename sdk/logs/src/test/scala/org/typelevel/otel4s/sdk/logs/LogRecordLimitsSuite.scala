package org.typelevel.otel4s.sdk.logs

import cats.Show
import cats.kernel.laws.discipline.HashTests
import cats.syntax.show._
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.Prop

class LogRecordLimitsSuite extends DisciplineSuite {

  private val logRecordLimitsGen: Gen[LogRecordLimits] =
    for {
      maxAttributes <- Gen.choose(0, 100)
      maxAttributeLength <- Gen.choose(0, 100)
    } yield LogRecordLimits.builder
      .withMaxNumberOfAttributes(maxAttributes)
      .withMaxAttributeValueLength(maxAttributeLength)
      .build

  private implicit val logRecordLimitsArbitrary: Arbitrary[LogRecordLimits] =
    Arbitrary(logRecordLimitsGen)

  private implicit val logRecordLimitsCogen: Cogen[LogRecordLimits] =
    Cogen[(Int, Int)].contramap(s => (s.maxNumberOfAttributes, s.maxAttributeValueLength))

  checkAll("LogRecordLimits.HashLaws", HashTests[LogRecordLimits].hash)

  property("Show[LogRecordLimits]") {
    Prop.forAll(logRecordLimitsGen) { s =>
      val expected = "LogRecordLimits{" +
        show"maxNumberOfAttributes=${s.maxNumberOfAttributes}, " +
        show"maxAttributeValueLength=${s.maxAttributeValueLength}}"

      assertEquals(Show[LogRecordLimits].show(s), expected)
      assertEquals(s.toString, expected)
    }
  }

}
