package com.jgh.octonions

import scala.language.implicitConversions

final class Quaternion(val w: Float, val x: Float, val y: Float, val z: Float)
    extends Serializable
    with IAlgebra[Quaternion] {

  def magnitude: Float = {
    Math.sqrt(x * x + y * y + z * z + w * w).toFloat
  }

  def normalized: Quaternion = {
    val d = magnitude
    new Quaternion(w / d, x / d, y / d, z / d)
  }

  def +(q: Quaternion): Quaternion = {
    new Quaternion(w + q.w, x + q.x, y + q.y, z + q.z)
  }

  def -(q: Quaternion): Quaternion = {
    new Quaternion(w - q.w, x - q.x, y - q.y, z - q.z)
  }

  def conjugate: Quaternion = {
    new Quaternion(w, -x, -y, -z)
  }

  def inv: Quaternion = {
    val mag = magnitude
    if (mag < 0.000001f) {
      Quaternion.Zero
    } else {
      conjugate / (mag * mag)
    }
  }

  def *(f: Float): Quaternion = {
    new Quaternion(w * f, x * f, y * f, z * f)
  }

  def /(f: Float): Quaternion = {
    new Quaternion(w / f, x / f, y / f, z / f)
  }

  def *(r: Quaternion): Quaternion = {
    val q = this
    val wm = q.w * r.w - q.x * r.x - q.y * r.y - q.z * r.z
    val xm = q.x * r.w + q.w * r.x + q.y * r.z - q.z * r.y
    val ym = q.w * r.y - q.x * r.z + q.y * r.w + q.z * r.x
    val zm = q.w * r.z + q.x * r.y - q.y * r.x + q.z * r.w
    new Quaternion(wm, xm, ym, zm)
  }

  override def toString: String = {
    "(" + w + ", " + x + ", " + y + ", " + z + ")"
  }

  override def equals(that: Any): Boolean = {
    that match {
      case that: Quaternion => this.hashCode == that.hashCode
      case _                => false
    }
  }

  override def hashCode: Int = {
    this.x.hashCode() ^ this.y.hashCode() << 2 ^ this.z.hashCode() >> 2 ^ this.w
      .hashCode() >> 1

  }

  override def /(t: Quaternion): Quaternion = {
    this * t.inv
  }
}

object Quaternion {

  def Identity = new Quaternion(1, 0, 0, 0)

  def Zero = new Quaternion(0, 0, 0, 0)

  implicit def floatToQuat(f: Float): Quaternion = new Quaternion(f, 0, 0, 0)

  implicit def intToQuat(i: Int): Quaternion = new Quaternion(i, 0, 0, 0)

}
