package simi.neurons

import akka.actor.{ActorRef, Actor, ActorLogging}
import simi.main._

import scala.collection.mutable

/**
 * Created by Simon on 19.04.2015.
 */
abstract class Neuron extends Actor with ActorLogging {
  val preConnections = mutable.MutableList[Connection]()
  var postConnections:mutable.MutableList[Connection] = _
  var activationValue:Int = _
  var currentActivationValue = 0
  var z_value = 0.0
  var a_value = 0.0
  var gradient = 0.0
  def increaseCurrentActivationValue() = currentActivationValue = currentActivationValue + 1
  def addValue(value:Double) = z_value  = z_value + value



  def updateWeights(m: Int, alpha: Double): Unit = {
    postConnections.foreach(C => {
      C.weight = C.weight - alpha * (gradient/m)
    })
  }

  def registerPreNeuron(preNeuron: ActorRef): Unit = {
    val connection = Connection(nextNeuron = context.self, prevNeuron = preNeuron)
    preConnections += connection
    activationValue = preConnections.size
    sender ! connection
  }


  def receive = {
    case NewInputValue(value) => newInputValue(value)
    case Y(y) => preConnections.foreach(C => C.prevNeuron ! DeltaImpuls(C.weight ,a_value - y))
    case DeltaImpuls(weight, delta) => calculateDeltaAndWeight(weight, delta)
    case UpdateWeights(m, size) => updateWeights(m, size)
    case RegisterPreNeuron(preNeuron) => registerPreNeuron(preNeuron)
    case SetPostConnections(postConnections) => this.postConnections = postConnections
    case x => log.info("Unknown Message: " + x)
  }

  def newInputValue(value: Double): Unit = {
    increaseCurrentActivationValue()
    addValue(value)
    if (currentActivationValue >= activationValue) {
      a_value = activationFunction()
      trigger()
      z_value = 0
      currentActivationValue = 0
    }
  }


  def calculateDeltaAndWeight(weight:Double, delta:Double) = {
    gradient = gradient + delta*a_value
    val newDelta = weight*delta *(a_value*(1-a_value))
    preConnections.foreach(C => {
        C.prevNeuron ! DeltaImpuls(weight, newDelta)
    })
    context.parent ! CountGradient()
  }

  /**
   * return a_value
   */
  def activationFunction():Double
  def trigger()

}
