package com.jgh.octonions

trait IAlgebra[T] {
  def magnitude: Float

  def normalized: T

  def +(t: T) : T

  def -(t: T) : T

  def *(t: T) : T

  def *(f: Float) : T

  def /(f : Float) : T

  def /(t : T) : T

  def inv : T

  def conjugate : T
}
