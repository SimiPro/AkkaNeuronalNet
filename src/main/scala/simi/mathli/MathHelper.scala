package simi.mathli

import scala.util.Random

/**
 * Created by Simon on 11.04.2015.
 */
object MathHelper {

  def divideEachElementInArray(x: Array[Double], value: Double): Array[Double] = {
    for (i <- 0 until x.size) {
      x(i) = x(i) / value
    }
    x
  }

  def divideEachElementInArray(x: Array[Array[Double]], value: Double): Array[Array[Double]] = {
    for (j <- 0 until x.size) {
      x(j) = divideEachElementInArray(x(j), value)
    }
    x
  }

  val alpha = 0.9

  def sigmoidGradient(z: Double) = {
    sigmoid(z) * (1 - sigmoid(z))
  }


  def cumulateArrays(x1: Array[Double], x2: Array[Double]): Array[Double] = {
    for (i <- 0 until x2.size) {
      x1(i) = x1(i) + x2(i)
    }
    x1
  }

  var random: Random = new Random()

  /*
  Sigmoid of z
   */
  def sigmoid(value: Double): Double = {
    val result = 1.0 / (1.0 + math.exp(-value))
    if (result < 0) {
      println("THIS FUCKING NOT ALLOWED TO HAPPEN")
    }
    result
  }

  /*
  Gets a number [-epsiolon;epsilon]
   */
  def randomInitializedNumber(): Double = {
    val epsilon = 0.0012
    random.nextDouble() //* 2 * epsilon - epsilon
  }
}