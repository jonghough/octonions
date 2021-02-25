package com.jgh.octonions

import org.scalatest.FlatSpec

class QuaternionTest extends FlatSpec {

  val epsilon = 1e-5f

  "1 * 1 " should " be 1" in {
    val a =
      new Quaternion(1, 0, 0, 0)
    assert(a * a == a)
  }

  "i * i " should " be -1" in {
    val a =
      new Quaternion(0, 1, 0, 0)
    assert(a * a == new Quaternion(-1, 0, 0, 0))
  }

  "i * j " should " be k" in {
    val e1 = new Quaternion(0, 1, 0, 0)
    val e2 = new Quaternion(0, 0, 1, 0)
    val e3 = new Quaternion(0, 0, 0, 1)
    assert(e1 * e2 == e3)
  }

  "i * k " should " be j" in {
    val e1 = new Quaternion(0, 1, 0, 0)
    val e2 = new Quaternion(0, 0, 0, 1)
    val e3 = new Quaternion(0, 0, 1, 0)
    assert(e1 * e2 == e3)
  }

  "j * k " should " be i" in {
    val e1 = new Quaternion(0, 0, 1, 0)
    val e2 = new Quaternion(0, 0, 0, 1)
    val e3 = new Quaternion(0, 1, 0, 0)
    assert(e1 * e2 == e3)
  }

  "i * j * k " should " be -1" in {
    val e1 = new Quaternion(0, 1, 0, 0)
    val e2 = new Quaternion(0, 0, 1, 0)
    val e3 = new Quaternion(0, 0, 0, 1)
    val e4 = new Quaternion(-1, 0, 0, 0)
    assert(e1 * e2 * e3 == e4)
  }

  "1 + 1 " should " be 2" in {
    val a =
      new Quaternion(1, 0, 0, 0)
    assert(a + a == new Quaternion(2, 0, 0, 0))
  }

  "i + j " should " be (0,1,1,0)" in {
    val i =
      new Quaternion(0, 1, 0, 0)
    val j =
      new Quaternion(0, 0, 1, 0)
    assert(i + j == new Quaternion(0, 1, 1, 0))
  }

  "i - j " should " be (0,1,-1,0)" in {
    val i =
      new Quaternion(0, 1, 0, 0)
    val j =
      new Quaternion(0, 0, 1, 0)
    assert(i - j == new Quaternion(0, 1, -1, 0))
  }

  "k - k " should " be zero" in {
    val k =
      new Quaternion(0, 0, 0, 1)
    assert(k - k == Quaternion.Zero)
  }

  "i*j - j " should "k - j = (0,0,-1,1)" in {
    val i =
      new Quaternion(0, 1, 0, 0)
    val j =
      new Quaternion(0, 0, 1, 0)
    assert(i * j - j == new Quaternion(0, 0, -1, 1))
  }

  "i*j*3 " should "3 * k" in {
    val i =
      new Quaternion(0, 1, 0, 0)
    val j =
      new Quaternion(0, 0, 1, 0)
    assert(i * j * 3 == new Quaternion(0, 0, 0, 3))
  }

  "3*i*j " should "3 * k" in {
    val i =
      new Quaternion(0, 1, 0, 0)
    val j =
      new Quaternion(0, 0, 1, 0)
    assert(3 * i * j == new Quaternion(0, 0, 0, 3))
  }

  "(1,2,3,4) * (5,6,7,8) " should " be (-60.0, 12.0, 30.0, 24.0)" in {
    val i =
      new Quaternion(1, 2, 3, 4)
    val j =
      new Quaternion(5, 6, 7, 8)
    val ij = i * j
    assert((ij - new Quaternion(-60.0f, 12.0f, 30.0f, 24.0f)).magnitude < 0.0001f)
  }
}
