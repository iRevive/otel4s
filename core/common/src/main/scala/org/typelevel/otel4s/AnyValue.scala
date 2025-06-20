/*
 * Copyright 2022 Typelevel
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

import cats.Hash
import cats.Show
import cats.syntax.show._

import scala.collection.immutable

/** Represents `any` type.
  *
  * @see
  *   [[https://opentelemetry.io/docs/specs/otel/logs/data-model/#type-any]]
  *
  * @see
  *   [[https://github.com/open-telemetry/opentelemetry-proto/blob/v1.5.0/opentelemetry/proto/common/v1/common.proto#L28-L40]]
  */
sealed trait AnyValue {

  override final def hashCode(): Int =
    Hash[AnyValue].hash(this)

  override final def equals(obj: Any): Boolean =
    obj match {
      case other: AnyValue => Hash[AnyValue].eqv(this, other)
      case _               => false
    }

  override final def toString: String =
    Show[AnyValue].show(this)

}

object AnyValue {

  def string(value: String): AnyValue = StringValue(value)
  def boolean(value: Boolean): AnyValue = BooleanValue(value)
  def long(value: Long): AnyValue = LongValue(value)
  def double(value: Double): AnyValue = DoubleValue(value)
  def bytes(value: Array[Byte]): AnyValue = ByteArrayValue(Array.copyOf(value, value.length))
  def array(values: immutable.Iterable[AnyValue]): AnyValue = ArrayValue(values)
  def map(values: Map[String, AnyValue]): AnyValue = MapValue(values)

  implicit val valueHash: Hash[AnyValue] = new Hash[AnyValue] {
    def hash(x: AnyValue): Int = x match {
      case StringValue(value)    => Hash[String].hash(value)
      case BooleanValue(value)   => Hash[Boolean].hash(value)
      case LongValue(value)      => Hash[Long].hash(value)
      case DoubleValue(value)    => Hash[Double].hash(value)
      case ByteArrayValue(value) => java.util.Arrays.hashCode(value)
      case ArrayValue(values)    => values.map(hash).hashCode()
      case MapValue(values)      => values.map { case (k, v) => (k, hash(v)) }.hashCode()
    }

    def eqv(x: AnyValue, y: AnyValue): Boolean = (x, y) match {
      case (StringValue(a), StringValue(b))   => a == b
      case (BooleanValue(a), BooleanValue(b)) => a == b
      case (LongValue(a), LongValue(b))       => a == b
      case (DoubleValue(a), DoubleValue(b))   => a == b
      case (ByteArrayValue(a), ByteArrayValue(b)) =>
        java.util.Arrays.equals(a, b)
      case (ArrayValue(a), ArrayValue(b)) =>
        a.size == b.size && a.lazyZip(b).forall { case (x, y) => eqv(x, y) }
      case (MapValue(a), MapValue(b)) =>
        a.size == b.size && a.keys.forall(k => b.contains(k) && eqv(a(k), b(k)))
      case _ => false
    }
  }

  implicit val valueShow: Show[AnyValue] = Show.show {
    case StringValue(value)    => show"StringValue($value)"
    case BooleanValue(value)   => show"BooleanValue($value)"
    case LongValue(value)      => show"LongValue($value)"
    case DoubleValue(value)    => show"DoubleValue($value)"
    case ByteArrayValue(value) => show"ByteArrayValue(${java.util.Arrays.toString(value)})"
    case ArrayValue(values)    => show"ArrayValue(${values.map(valueShow.show).mkString("[", ", ", "]")})"
    case MapValue(values) =>
      show"MapValue(${values.map { case (k, v) => s"$k -> ${valueShow.show(v)}" }.mkString("{", ", ", "}")})"
  }

  private[otel4s] final case class StringValue(value: String) extends AnyValue
  private[otel4s] final case class BooleanValue(value: Boolean) extends AnyValue
  private[otel4s] final case class LongValue(value: Long) extends AnyValue
  private[otel4s] final case class DoubleValue(value: Double) extends AnyValue
  private[otel4s] final case class ByteArrayValue(value: Array[Byte]) extends AnyValue
  private[otel4s] final case class ArrayValue(values: immutable.Iterable[AnyValue]) extends AnyValue
  private[otel4s] final case class MapValue(values: Map[String, AnyValue]) extends AnyValue

}
