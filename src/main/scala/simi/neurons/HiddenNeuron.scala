package simi.neurons

import simi.main.NewInputValue
import simi.mathli.MathHelper

/**
 * Created by Simon on 19.04.2015.
 */
class HiddenNeuron extends  Neuron {
  override def trigger(): Unit = {
    postConnections.foreach(C => {
      C.nextNeuron ! NewInputValue(a_value*C.weight)
    })
  }

  override def activationFunction(): Double = {
    MathHelper.sigmoid(z_value)
  }


}
