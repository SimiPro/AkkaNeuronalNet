package simi.neurons

import simi.main.NewInputValue

/**
 * Created by Simon on 19.04.2015.
 */
class BiasNeuron() extends  Neuron {

  override def trigger(): Unit = {
    postConnections.foreach(C => {
      C.nextNeuron ! NewInputValue(a_value*C.weight)
    })
  }

  override def activationFunction(): Double = {
    1
  }
}
