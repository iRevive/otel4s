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
package benchmarks

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

// benchmarks/Jmh/run org.typelevel.otel4s.benchmarks.AttributesOpsBenchmark -prof gc
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Measurement(iterations = 15, time = 1)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1)
class AttributesOpsBenchmark {

  @Param(Array("1", "5", "25"/* "10", "25", "100"*/))
  var attributesSize: Int = _
  var attributes: Attributes = _
  val minKey: String = "key-1"
  var maxKey: String = _

  @Benchmark
  def builder(): Attributes = {
    val builder = Attributes.newBuilder
    for (i <- 0 until attributesSize) {
      builder.addOne(s"key-$i", "value")
    }
    builder.result()
  }

  @Benchmark
  def lookupGetMin(): Option[Attribute[String]] =
    attributes.get[String](minKey)

  @Benchmark
  def lookupGetMax(): Option[Attribute[String]] =
    attributes.get[String](maxKey)

  @Benchmark
  def lookupFindMin(): Option[Attribute[_]] =
    attributes.find(_.key.name == minKey)

  @Benchmark
  def lookupFindMax(): Option[Attribute[_]] =
    attributes.find(_.key.name == maxKey)

  @Benchmark
  def added(): Attributes =
    attributes.added("a", "a").added("b", "b").added("c", "c")

  @Benchmark
  def addedDuplicatesMin(): Attributes =
    attributes.added(minKey, "a").added("b", "b").added("c", "c")

  @Benchmark
  def addedDuplicatesMax(): Attributes =
    attributes.added(minKey, "a").added("b", "b").added(maxKey, "c")

  @Benchmark
  def concat(): Attributes =
    attributes.concat(attributes)

  @Setup(Level.Trial)
  def setUp(): Unit = {
    maxKey = s"key-$attributesSize"
    attributes = 1
      .to(attributesSize)
      .map(i => Attribute(s"key-$i", s"value-$i"))
      .to(Attributes)
  }

}
