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
