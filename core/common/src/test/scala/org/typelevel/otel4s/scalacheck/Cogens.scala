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

package org.typelevel.otel4s.scalacheck

import org.scalacheck.Cogen
import org.scalacheck.rng.Seed
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.AttributeType
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.Value

trait Cogens {

  implicit val attributeTypeCogen: Cogen[AttributeType[_]] =
    Cogen[String].contramap(_.toString)

  implicit def attributeKeyCogen[A]: Cogen[AttributeKey[A]] =
    Cogen[(String, String)].contramap[AttributeKey[A]] { attribute =>
      (attribute.name, attribute.`type`.toString)
    }

  implicit def attributeCogen[A: Cogen]: Cogen[Attribute[A]] =
    Cogen[(AttributeKey[A], A)].contramap(a => (a.key, a.value))

  implicit val attributeExistentialCogen: Cogen[Attribute[_]] =
    Cogen { (seed, attr) =>
      def primitive[A: Cogen](seed: Seed): Seed =
        Cogen[A].perturb(seed, attr.value.asInstanceOf[A])

      def seq[A: Cogen](seed: Seed): Seed =
        Cogen[Seq[A]].perturb(seed, attr.value.asInstanceOf[Seq[A]])

      val valueCogen: Seed => Seed = attr.key.`type` match {
        case AttributeType.Boolean    => primitive[Boolean]
        case AttributeType.Double     => primitive[Double]
        case AttributeType.String     => primitive[String]
        case AttributeType.Long       => primitive[Long]
        case AttributeType.BooleanSeq => seq[Boolean]
        case AttributeType.DoubleSeq  => seq[Double]
        case AttributeType.StringSeq  => seq[String]
        case AttributeType.LongSeq    => seq[Long]
      }

      valueCogen(attributeKeyCogen.perturb(seed, attr.key))
    }

  implicit val attributesCogen: Cogen[Attributes] =
    Cogen[List[Attribute[_]]].contramap(_.toList)

  implicit val valueCogen: Cogen[Value] =
    Cogen { (seed, value) =>
      value match {
        case Value.StringValue(value)    => Cogen[String].perturb(seed, value)
        case Value.BooleanValue(value)   => Cogen[Boolean].perturb(seed, value)
        case Value.LongValue(value)      => Cogen[Long].perturb(seed, value)
        case Value.DoubleValue(value)    => Cogen[Double].perturb(seed, value)
        case Value.ByteArrayValue(value) => Cogen[Array[Byte]].perturb(seed, value)
        case Value.ArrayValue(values)    => values.foldLeft(seed)((s, v) => valueCogen.perturb(s, v))
        case Value.MapValue(values) =>
          values.foldLeft(seed) { case (s, (k, v)) => valueCogen.perturb(Cogen[String].perturb(s, k), v) }
      }
    }

}

object Cogens extends Cogens
