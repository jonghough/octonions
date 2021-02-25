package com.jgh.octonions


final class Sedenion(
    val e0: Float,
    val e1: Float,
    val e2: Float,
    val e3: Float,
    val e4: Float,
    val e5: Float,
    val e6: Float,
    val e7: Float,
    val e8: Float,
    val e9: Float,
    val e10: Float,
    val e11: Float,
    val e12: Float,
    val e13: Float,
    val e14: Float,
    val e15: Float
) extends IAlgebra[Sedenion] {

  def this() = {
    this(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  }

  def +(o: Sedenion): Sedenion = {
    new Sedenion(
      e0 + o.e0,
      e1 + o.e1,
      e2 + o.e2,
      e3 + o.e3,
      e4 + o.e4,
      e5 + o.e5,
      e6 + o.e6,
      e7 + o.e7,
      e8 + o.e8,
      e9 + o.e9,
      e10 + o.e10,
      e11 + o.e11,
      e12 + o.e12,
      e13 + o.e13,
      e14 + o.e14,
      e15 + o.e15
    )
  }

  def -(o: Sedenion): Sedenion = {
    new Sedenion(
      e0 - o.e0,
      e1 - o.e1,
      e2 - o.e2,
      e3 - o.e3,
      e4 - o.e4,
      e5 - o.e5,
      e6 - o.e6,
      e7 - o.e7,
      e8 - o.e8,
      e9 - o.e9,
      e10 - o.e10,
      e11 - o.e11,
      e12 - o.e12,
      e13 - o.e13,
      e14 - o.e14,
      e15 - o.e15
    )
  }

  def *(f: Float): Sedenion = {
    new Sedenion(
      e0 * f,
      e1 * f,
      e2 * f,
      e3 * f,
      e4 * f,
      e5 * f,
      e6 * f,
      e7 * f,
      e8 * f,
      e9 * f,
      e10 * f,
      e11 * f,
      e12 * f,
      e13 * f,
      e14 * f,
      e15 * f
    )
  }

  def *(o: Sedenion): Sedenion = {
    val o1 = new Octonion(e0, e1, e2, e3, e4, e5, e6, e7)
    val o2 = new Octonion(e8, e9, e10, e11, e12, e13, e14, e15)
    val o3 = new Octonion(o.e0, o.e1, o.e2, o.e3, o.e4, o.e5, o.e6, o.e7)
    val o4 = new Octonion(o.e8, o.e9, o.e10, o.e11, o.e12, o.e13, o.e14, o.e15)

    val r1 = o1 * o3 - (o4.conjugate * o2)
    val r2 = o4 * o1 + (o2 * o3.conjugate)
    new Sedenion(
      r1.e0,
      r1.e1,
      r1.e2,
      r1.e3,
      r1.e4,
      r1.e5,
      r1.e6,
      r1.e7,
      r2.e0,
      r2.e1,
      r2.e2,
      r2.e3,
      r2.e4,
      r2.e5,
      r2.e6,
      r2.e7
    )
  }

  def conjugate: Sedenion = {
    new Sedenion(
      e0,
      -e1,
      -e2,
      -e3,
      -e4,
      -e5,
      -e6,
      -e7,
      -e8,
      -e9,
      -e10,
      -e11,
      -e12,
      -e13,
      -e14,
      -e15
    )
  }

  override def toString: String = {
    "(" +
      e0 + ", " + e1 + ", " + e2 + ", " + e3 + ", " + e4 + ", " + e5 + ", " + e6 + ", " + e7 + ", " +
      e8 + ", " + e9 + ", " + e10 + ", " + e11 + ", " + e12 + ", " + e13 + ", " + e14 + ", " + e15 +
      ")"
  }

  override def equals(that: Any): Boolean = {
    that match {
      case that: Sedenion => this.hashCode == that.hashCode
      case _              => false
    }
  }

  override def hashCode: Int = {
    val d1: Int = (this.e0.hashCode() * 7) ^ (this.e9
      .hashCode() << 2) - (this.e2.hashCode() * 27) * (this.e11.hashCode() >> 1)
    val d2: Int = (this.e8.hashCode() * 11) ^ (this.e1
      .hashCode() << 2) - (this.e10.hashCode() * 31) * (this.e3.hashCode() >> 2)
    val r1: Int = (this.e4.hashCode() >> 1) ^ (this.e12
      .hashCode() << 3) + (this.e5.hashCode() << 1) * 131
    val r2: Int = (2 * this.e13.hashCode() >> 1) ^ (this.e4
      .hashCode() << 2) + (this.e13.hashCode() << 2) * 141
    val q1: Int = (this.e6.hashCode() * 11 - this.e7.hashCode()) << 1
    val q2: Int = (this.e14.hashCode() * 12 - this.e15.hashCode()) << 3
    val es: Int = (q1 << 1) * (q2 >> 1) - 13321
    val em: Int = d1 + d2 + (r1 - r2) + es
    ((d1 >> 1) & (d2 << 3)) + (em * 92281) - q1 * 3 + ((q1 << 2) * q2)
  }

  override def magnitude: Float = {
    Math
      .sqrt(
        e0 * e0 + e1 * e1 + e2 * e2 + e3 * e3 + e4 * e4 + e5 * e5 + e6 * e6 + e7 * e7 +
          e8 * e8 + e9 * e9 + e10 * e10 + e11 * e11 + e12 * e12 + e13 * e13 + e14 * e14 + e15 * e15
      )
      .toFloat
  }

  override def normalized: Sedenion = {
    val d = magnitude
    this / d
  }

  override def /(f: Float): Sedenion = {
    new Sedenion(
      e0 / f,
      e1 / f,
      e2 / f,
      e3 / f,
      e4 / f,
      e5 / f,
      e6 / f,
      e7 / f,
      e8 / f,
      e9 / f,
      e10 / f,
      e11 / f,
      e12 / f,
      e13 / f,
      e14 / f,
      e15 / f
    )
  }

  override def inv: Sedenion = {
    val mag = magnitude
    if (mag < 0.000001f) {
      Sedenion.Zero
    } else {
      conjugate / (mag * mag)
    }
  }

  override def /(t: Sedenion): Sedenion = {
    this * t.inv
  }
}

object Sedenion {

  def Identity = new Sedenion(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  def Zero = new Sedenion(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  implicit def floatToSed(f: Float): Sedenion =
    new Sedenion(f, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

  implicit def intToSed(i: Int): Sedenion =
    new Sedenion(i, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

  def distSq(o1: Sedenion, o2: Sedenion): Float = {
    (o1.e0 - o2.e0) * (o1.e0 - o2.e0) +
      (o1.e1 - o2.e1) * (o1.e1 - o2.e1) +
      (o1.e2 - o2.e2) * (o1.e2 - o2.e2) +
      (o1.e3 - o2.e3) * (o1.e3 - o2.e3) +
      (o1.e4 - o2.e4) * (o1.e4 - o2.e4) +
      (o1.e5 - o2.e5) * (o1.e5 - o2.e5) +
      (o1.e6 - o2.e6) * (o1.e6 - o2.e6) +
      (o1.e7 - o2.e7) * (o1.e7 - o2.e7) +
      (o1.e8 - o2.e8) * (o1.e8 - o2.e8) +
      (o1.e9 - o2.e9) * (o1.e9 - o2.e9) +
      (o1.e10 - o2.e10) * (o1.e10 - o2.e10) +
      (o1.e11 - o2.e11) * (o1.e11 - o2.e11) +
      (o1.e12 - o2.e12) * (o1.e12 - o2.e12) +
      (o1.e13 - o2.e13) * (o1.e13 - o2.e13) +
      (o1.e14 - o2.e14) * (o1.e14 - o2.e14) +
      (o1.e15 - o2.e15) * (o1.e15 - o2.e15)
  }
}
