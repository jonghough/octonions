package com.jgh.octonions

import org.scalatest.FlatSpec


class SedenionTest extends FlatSpec {

  val epsilon = 1e-5f

  "1 * 1 " should " be 1" in {
    val a = new Sedenion(1, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)
    assert(a * a == a)
  }

  "1 * e1 " should " be e1" in {
    val a = new Sedenion(1, 0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)
    val e1 = new Sedenion(0,1, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)
    assert(a * e1 == e1)
  }

  "e1 * e2 " should " be e3" in {
    val e1 =  new Sedenion(0,1, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)
    val e2 =  new Sedenion(0,0, 1, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)
    val e3 =  new Sedenion(0,0, 0, 1, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)
    assert(e1 * e2 == e3)
  }

  "e2 * e1 " should " be -e3" in {
    val e1 =  new Sedenion(0,1, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)
    val e2 =  new Sedenion(0,0, 1, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)
    val e3 =  new Sedenion(0,0, 0, 1, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)
    assert(Sedenion.distSq(e2 * e1, e3 * -1f) < epsilon)
  }

  "e2 + e1 " should " be (0,1,1,0,0,0,0,0)" in {
    val e1 =  new Sedenion(0,1, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)
    val e2 =  new Sedenion(0,0, 1, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)
    val e3 =  new Sedenion(0,1, 1, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0)
    assert(e1 + e2 == e3)
  }

  "e1 * inverse" should " be the identity" in {
    val e1 =  new Sedenion(1,1, 0,-1, 0, 0, -1, 0, 1,0,1,-1,0,1,1,-1) 
    val i = e1.inv
    println(i)
    assert(e1 * i == new Sedenion(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
  }

  "(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) * (1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) " should " be (-104.0, 14.0, 12.0, 10.0, -176.0, -10.0, 4.0, -30.0)" in {
    val i =
      new Sedenion(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
    val j =
      new Sedenion(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
    val ij = i * j
    println(ij)
    assert((ij - new Sedenion(-14.0f, 2.0f, 2.0f, 2.0f, 2.0f, 2.0f, 2.0f, 2.0f, 2.0f, 2.0f, 2.0f, 2.0f, 2.0f, 2.0f, 2.0f, 2.0f)).magnitude < 0.0001f)
  }

    "e4 * e15 " should " be -e11" in {
    val e4 =  new Sedenion(0,0, 0, 0, 1, 0, 0, 0, 0,0,0,0,0,0,0,0)
    val e15 =  new Sedenion(0,0, 0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,1)
    val e11 =  new Sedenion(0,0, 0, 0, 0, 0, 0, 0, 0,0,0,1,0,0,0,0) 
    assert(Sedenion.distSq(e4 * e15, e11* -1f) < epsilon)
  }
}
