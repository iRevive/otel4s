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
package internal

import cats.Hash
import cats.Show
import cats.syntax.foldable._
import org.typelevel.ci.CIString

/** A description of an instrument that was registered to record measurements.
  */
private[metrics] sealed trait InstrumentDescriptor {

  /** The name of the instrument.
    *
    * @see
    *   [[https://opentelemetry.io/docs/specs/otel/metrics/sdk/#name-conflict]]
    */
  def name: CIString

  /** The description of the instrument.
    */
  def description: Option[String]

  /** The unit of the instrument.
    */
  def unit: Option[String]

  /** The type of the instrument.
    */
  def instrumentType: InstrumentType

  def advice: Advice

  override final def hashCode(): Int =
    Hash[InstrumentDescriptor].hash(this)

  override final def equals(obj: Any): Boolean =
    obj match {
      case other: InstrumentDescriptor =>
        Hash[InstrumentDescriptor].eqv(this, other)
      case _ => false
    }

  override final def toString: String =
    Show[InstrumentDescriptor].show(this)
}

private[metrics] object InstrumentDescriptor {

  sealed trait Synchronous extends InstrumentDescriptor {
    def instrumentType: InstrumentType.Synchronous
  }

  sealed trait Asynchronous extends InstrumentDescriptor {
    def instrumentType: InstrumentType.Asynchronous
  }

  /** Creates an [[InstrumentDescriptor]] for a synchronous instrument.
    */
  def synchronous(
      name: CIString,
      description: Option[String],
      unit: Option[String],
      instrumentType: InstrumentType.Synchronous,
      advice: Advice
  ): InstrumentDescriptor.Synchronous =
    SynchronousImpl(name, description, unit, instrumentType, advice)

  /** Creates an [[InstrumentDescriptor]] for a asynchronous instrument.
    */
  def asynchronous(
      name: CIString,
      description: Option[String],
      unit: Option[String],
      instrumentType: InstrumentType.Asynchronous,
      advice: Advice
  ): InstrumentDescriptor.Asynchronous =
    AsynchronousImpl(name, description, unit, instrumentType, advice)

  // advice is not a part of the hash, it's intended
  implicit val instrumentDescriptorHash: Hash[InstrumentDescriptor] =
    Hash.by { descriptor =>
      (
        descriptor.name,
        descriptor.description,
        descriptor.unit,
        descriptor.instrumentType
      )
    }

  implicit val instrumentDescriptorShow: Show[InstrumentDescriptor] =
    Show.show { descriptor =>
      val description = descriptor.description.foldMap(d => s"description=$d, ")
      val unit = descriptor.unit.foldMap(d => s"unit=$d, ")
      s"InstrumentDescriptor{name=${descriptor.name}, $description$unit" +
        s"type=${descriptor.instrumentType}, " +
        s"advice=${descriptor.advice}}"
    }

  private final case class SynchronousImpl(
      name: CIString,
      description: Option[String],
      unit: Option[String],
      instrumentType: InstrumentType.Synchronous,
      advice: Advice
  ) extends InstrumentDescriptor.Synchronous

  private final case class AsynchronousImpl(
      name: CIString,
      description: Option[String],
      unit: Option[String],
      instrumentType: InstrumentType.Asynchronous,
      advice: Advice
  ) extends InstrumentDescriptor.Asynchronous

}
