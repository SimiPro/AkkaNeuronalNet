package simi.layer

import akka.actor.Props
import simi.neurons.{BiasNeuron, HiddenNeuron}

/**
 * Created by Simon on 19.04.2015.
 */
class HiddenLayer(units:Int) extends Layer(units) {
  override def createNeurons(): Unit = {
    neurons(0) = context.actorOf(Props(new BiasNeuron), "biasneuron")
    for (i <- 1  until units) {
      neurons(i) = context.actorOf(Props(new HiddenNeuron), "hiddenneuron" + i)
    }
  }
}
