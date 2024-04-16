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

package org.typelevel.otel4s.sdk.metrics.view

import munit.ScalaCheckSuite

class ViewRegistrySuite extends ScalaCheckSuite {

  import ViewRegistry.toGlobPattern

  test("correctly create a pattern") {
    assertEquals(toGlobPattern("foo").apply("foo"), true)
    assertEquals(toGlobPattern("foo").apply("Foo"), true)
    assertEquals(toGlobPattern("foo").apply("bar"), false)
    assertEquals(toGlobPattern("fo?").apply("foo"), true)
    assertEquals(toGlobPattern("fo??bar").apply("fooo"), false)
    assertEquals(toGlobPattern("fo??bar").apply("fooobar"), true)
    assertEquals(toGlobPattern("fo?").apply("fob"), true)
    assertEquals(toGlobPattern("fo?").apply("fooo"), false)
    assertEquals(toGlobPattern("*").apply("foo"), true)
    assertEquals(toGlobPattern("*").apply("bar"), true)
    assertEquals(toGlobPattern("*").apply("baz"), true)
    assertEquals(toGlobPattern("*").apply("foo.bar.baz"), true)
    assertEquals(toGlobPattern("fo*").apply("fo"), true)
    assertEquals(toGlobPattern("fo*").apply("foo"), true)
    assertEquals(toGlobPattern("fo*").apply("fooo"), true)
    assertEquals(toGlobPattern("fo*bar").apply("foo.bar.baz"), false)
    assertEquals(toGlobPattern("fo*bar").apply("foo.bar.baz.bar"), true)
    assertEquals(toGlobPattern("f()[]$^.{}|").apply("f()[]$^.{}|"), true)
    assertEquals(toGlobPattern("f()[]$^.{}|?").apply("f()[]$^.{}|o"), true)
    assertEquals(toGlobPattern("f()[]$^.{}|*").apply("f()[]$^.{}|ooo"), true)

  }

}
