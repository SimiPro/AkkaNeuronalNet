package simi.neurons

import akka.actor.ActorLogging
import simi.main.SendResult
import simi.mathli.MathHelper

/**
 * Created by Simon on 19.04.2015.
 */
class OutputNeuron(unitsOfNextLayer:Int,unitsOnPrevLayer:Int, index:Int) extends  Neuron with ActorLogging {
  override def trigger(): Unit = {
    log.debug("Result: " + a_value)
    context.parent ! SendResult(a_value, index)
  }

  /**
   * return a_value
   */
  override def activationFunction(): Double = {
    MathHelper.sigmoid(z_value)
  }

}
