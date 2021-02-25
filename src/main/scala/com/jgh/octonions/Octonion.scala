package com.jgh.octonions

 
final class Octonion(
    val e0: Float,
    val e1: Float,
    val e2: Float,
    val e3: Float,
    val e4: Float,
    val e5: Float,
    val e6: Float,
    val e7: Float
) extends IAlgebra[Octonion] {

  def this() = {
    this(0, 0, 0, 0, 0, 0, 0, 0)
  }

  def +(o: Octonion): Octonion = {
    new Octonion(
      e0 + o.e0,
      e1 + o.e1,
      e2 + o.e2,
      e3 + o.e3,
      e4 + o.e4,
      e5 + o.e5,
      e6 + o.e6,
      e7 + o.e7
    )
  }

  def -(o: Octonion): Octonion = {
    new Octonion(
      e0 - o.e0,
      e1 - o.e1,
      e2 - o.e2,
      e3 - o.e3,
      e4 - o.e4,
      e5 - o.e5,
      e6 - o.e6,
      e7 - o.e7
    )
  }

  def *(f: Float): Octonion = {
    new Octonion(e0 * f, e1 * f, e2 * f, e3 * f, e4 * f, e5 * f, e6 * f, e7 * f)
  }

  def *(o: Octonion): Octonion = {
    val q1 = new Quaternion(e0, e1, e2, e3)
    val q2 = new Quaternion(e4, e5, e6, e7)
    val q3 = new Quaternion(o.e0, o.e1, o.e2, o.e3)
    val q4 = new Quaternion(o.e4, o.e5, o.e6, o.e7)

    val r1 = q1 * q3 - (q4.conjugate * q2)
    val r2 = q4 * q1 + (q2 * q3.conjugate)
    new Octonion(r1.w, r1.x, r1.y, r1.z, r2.w, r2.x, r2.y, r2.z)
  }

  def conjugate: Octonion = {
    new Octonion(e0, -e1, -e2, -e3, -e4, -e5, -e6, -e7)
  }

  override def toString: String = {
    "(" + e0 + ", " + e1 + ", " + e2 + ", " + e3 + ", " + e4 + ", " + e5 + ", " + e6 + ", " + e7 + ")"
  }

  override def equals(that: Any): Boolean = {
    that match {
      case that: Octonion => this.hashCode == that.hashCode
      case _              => false
    }
  }

  override def hashCode: Int = {
    val d = (this.e0.hashCode() * 7) ^ (this.e1
      .hashCode() << 2) - (this.e2.hashCode() * 27) * (this.e3.hashCode() >> 1)
    val r = (this.e4.hashCode() >> 1) ^ (this.e4
      .hashCode() << 3) + (this.e5.hashCode() << 1) * 131
    val q = (this.e6.hashCode() * 11 - this.e7.hashCode()) << 1
    ((d >> 1) & (d << 3)) + (r * 92281) - q * 3
  }

  override def magnitude: Float = {
    Math
      .sqrt(
        e0 * e0 + e1 * e1 + e2 * e2 + e3 * e3 + e4 * e4 + e5 * e5 + e6 * e6 + e7 * e7
      )
      .toFloat
  }

  override def normalized: Octonion = {
    val d = magnitude
    this / d
  }

  override def /(f: Float): Octonion = {
    new Octonion(e0 / f, e1 / f, e2 / f, e3 / f, e4 / f, e5 / f, e6 / f, e7 / f)
  }

  override def inv: Octonion = {
    val mag = magnitude
    if (mag < 0.000001f) {
      Octonion.Zero
    } else {
      conjugate / (mag * mag)
    }
  }

  override def /(t: Octonion): Octonion = {
    this * t.inv
  }
}

object Octonion {

  def Identity = new Octonion(1, 0, 0, 0, 0, 0, 0, 0)

  def Zero = new Octonion(0, 0, 0, 0, 0, 0, 0, 0)

  implicit def floatToOct(f: Float): Octonion =
    new Octonion(f, 1, 1, 1, 1, 1, 1, 1)

  implicit def intToOct(i: Int): Octonion = new Octonion(i, 1, 1, 1, 1, 1, 1, 1)

  def distSq(o1: Octonion, o2: Octonion): Float = {
    (o1.e0 - o2.e0) * (o1.e0 - o2.e0) +
      (o1.e1 - o2.e1) * (o1.e1 - o2.e1) +
      (o1.e2 - o2.e2) * (o1.e2 - o2.e2) +
      (o1.e3 - o2.e3) * (o1.e3 - o2.e3) +
      (o1.e4 - o2.e4) * (o1.e4 - o2.e4) +
      (o1.e5 - o2.e5) * (o1.e5 - o2.e5) +
      (o1.e6 - o2.e6) * (o1.e6 - o2.e6) +
      (o1.e7 - o2.e7) * (o1.e7 - o2.e7)
  }
}
