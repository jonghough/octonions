package com.jgh.octonions

import org.scalatest.FlatSpec


class OctonionTest extends FlatSpec {

  val epsilon = 1e-5f

  "1 * 1 " should " be 1" in {
    val a = new Octonion(1, 0, 0, 0, 0, 0, 0, 0)
    assert(a * a == a)
  }

  "1 * e1 " should " be e1" in {
    val a = new Octonion(1, 0, 0, 0, 0, 0, 0, 0)
    val e1 = new Octonion(0, 1, 0, 0, 0, 0, 0, 0)
    assert(a * e1 == e1)
  }

  "e1 * e2 " should " be e3" in {
    val e1 = new Octonion(0, 1, 0, 0, 0, 0, 0, 0)
    val e2 = new Octonion(0, 0, 1, 0, 0, 0, 0, 0)
    val e3 = new Octonion(0, 0, 0, 1, 0, 0, 0, 0)
    assert(e1 * e2 == e3)
  }

  "e2 * e1 " should " be -e3" in {
    val e1 = new Octonion(0, 1, 0, 0, 0, 0, 0, 0)
    val e2 = new Octonion(0, 0, 1, 0, 0, 0, 0, 0)
    val e3 = new Octonion(0, 0, 0, 1, 0, 0, 0, 0)
    assert(Octonion.distSq(e2 * e1, e3 * -1f) < epsilon)
  }

  "e2 + e1 " should " be (0,1,1,0,0,0,0,0)" in {
    val e1 = new Octonion(0, 1, 0, 0, 0, 0, 0, 0)
    val e2 = new Octonion(0, 0, 1, 0, 0, 0, 0, 0)
    assert(e1 + e2 == new Octonion(0, 1, 1, 0, 0, 0, 0, 0))
  }

  "(1,2,3,4,5,6,7,8) * (8,7,6,5,4,3,2,1) " should " be (-104.0, 14.0, 12.0, 10.0, 152.0, 42.0, 4.0, 74.0)" in {
    val i =
      new Octonion(1, 2, 3, 4, 5, 6, 7, 8)
    val j =
      new Octonion(8, 7, 6, 5, 4, 3, 2, 1)
    val ij = i * j 
    assert((ij - new Octonion(-104.0f, 14.0f, 12.0f, 10.0f, 152.0f, 42.0f, 4.0f, 74.0f)).magnitude < 0.0001f)
  }
}
