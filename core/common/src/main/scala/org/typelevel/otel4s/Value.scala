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
sealed trait Value {

  override final def hashCode(): Int =
    Hash[Value].hash(this)

  override final def equals(obj: Any): Boolean =
    obj match {
      case other: Value => Hash[Value].eqv(this, other)
      case _            => false
    }

  override final def toString: String =
    Show[Value].show(this)

}

object Value {

  def string(value: String): Value = StringValue(value)
  def boolean(value: Boolean): Value = BooleanValue(value)
  def long(value: Long): Value = LongValue(value)
  def double(value: Double): Value = DoubleValue(value)
  def bytes(value: Array[Byte]): Value = ByteArrayValue(Array.copyOf(value, value.length))
  def array(values: immutable.Iterable[Value]): Value = ArrayValue(values)
  def map(values: Map[String, Value]): Value = MapValue(values)

  private[otel4s] final case class StringValue(value: String) extends Value
  private[otel4s] final case class BooleanValue(value: Boolean) extends Value
  private[otel4s] final case class LongValue(value: Long) extends Value
  private[otel4s] final case class DoubleValue(value: Double) extends Value
  private[otel4s] final case class ByteArrayValue(value: Array[Byte]) extends Value
  private[otel4s] final case class ArrayValue(values: immutable.Iterable[Value]) extends Value
  private[otel4s] final case class MapValue(values: Map[String, Value]) extends Value

  implicit val valueHash: Hash[Value] = new Hash[Value] {
    def hash(x: Value): Int = x match {
      case StringValue(value)    => Hash[String].hash(value)
      case BooleanValue(value)   => Hash[Boolean].hash(value)
      case LongValue(value)      => Hash[Long].hash(value)
      case DoubleValue(value)    => Hash[Double].hash(value)
      case ByteArrayValue(value) => java.util.Arrays.hashCode(value)
      case ArrayValue(values)    => values.map(hash).hashCode()
      case MapValue(values)      => values.map { case (k, v) => (k, hash(v)) }.hashCode()
    }

    def eqv(x: Value, y: Value): Boolean = (x, y) match {
      case (StringValue(a), StringValue(b))       => a == b
      case (BooleanValue(a), BooleanValue(b))     => a == b
      case (LongValue(a), LongValue(b))           => a == b
      case (DoubleValue(a), DoubleValue(b))       => a == b
      case (ByteArrayValue(a), ByteArrayValue(b)) => java.util.Arrays.equals(a, b)
      case (ArrayValue(a), ArrayValue(b)) =>
        a.size == b.size && a.lazyZip(b).forall { case (x, y) => eqv(x, y) }
      case (MapValue(a), MapValue(b)) =>
        a.size == b.size && a.keys.forall(k => b.contains(k) && eqv(a(k), b(k))) // todo: doesn't seem to be correct
      case _ => false
    }
  }

  /** Show instance for Value */
  implicit val valueShow: Show[Value] = Show.show {
    case StringValue(value)    => show"StringValue($value)"
    case BooleanValue(value)   => show"BooleanValue($value)"
    case LongValue(value)      => show"LongValue($value)"
    case DoubleValue(value)    => show"DoubleValue($value)"
    case ByteArrayValue(value) => show"ByteArrayValue(${java.util.Arrays.toString(value)})"
    case ArrayValue(values)    => show"ArrayValue(${values.map(valueShow.show).mkString("[", ", ", "]")})"
    case MapValue(values) =>
      show"MapValue(${values.map { case (k, v) => s"$k -> ${valueShow.show(v)}" }.mkString("{", ", ", "}")})"
  }

}
