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

package org.typelevel.otel4s.sdk.metrics.internal

import cats.Hash
import cats.Show
import cats.syntax.foldable._
import org.typelevel.otel4s.AttributeKey
import org.typelevel.otel4s.metrics.BucketBoundaries

/** Advisory options influencing aggregation configuration parameters.
  *
  * @see
  *   [[https://opentelemetry.io/docs/specs/otel/metrics/api/#instrument-advisory-parameters]]
  */
private[metrics] sealed trait Advice {

  /** The recommended set of bucket boundaries to use if the selected
    * aggregation is `ExplicitBucketHistogram`.
    */
  def explicitBucketBoundaries: Option[BucketBoundaries]

  /** The list of the attribute keys to be used for the resulting instrument.
   */
  def attributeKeys: Option[Set[AttributeKey[_]]]

  override final def hashCode(): Int =
    Hash[Advice].hash(this)

  override final def equals(obj: Any): Boolean =
    obj match {
      case other: Advice => Hash[Advice].eqv(this, other)
      case _             => false
    }

  override final def toString: String =
    Show[Advice].show(this)
}

private[metrics] object Advice {

  /** Creates an [[Advice]] with the given values.
    *
    * @param bucketBoundaries
    *   the recommended set of bucket boundaries to use if the selected
    *   aggregation is `explicit bucket histogram`
    */
  def apply(bucketBoundaries: Option[BucketBoundaries]): Advice =
    Impl(bucketBoundaries, None)

  implicit val adviceHash: Hash[Advice] =
    Hash.by(a => (a.explicitBucketBoundaries, a.attributeKeys))

  implicit val adviceShow: Show[Advice] =
    Show.show { advice =>
      val boundaries = advice.explicitBucketBoundaries.map { b =>
        s"explicitBucketBoundaries=$b"
      }

      val attributeKeys = advice.attributeKeys.map { k =>
        s"attributeKeys=${k.mkString("[", ",", "]")}"
      }

      Vector(attributeKeys, boundaries).flatten.mkString("Advice{", ", ", "}")
    }

  private final case class Impl(
      explicitBucketBoundaries: Option[BucketBoundaries],
      attributeKeys: Option[Set[AttributeKey[_]]]
  ) extends Advice
}
