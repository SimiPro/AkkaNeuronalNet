package simi.layer

import akka.actor.{ActorRef, Props}
import akka.pattern.pipe

import simi.main._
import simi.neurons.{InputNeuron, BiasNeuron}
import scala.concurrent.{Future, Promise}

/**
 * Created by Simon on 19.04.2015.
 */
class InputLayer(units:Int) extends Layer(units) {
  import context.dispatcher

  var gradientCounter = 0
  var gradientP:Promise[Boolean] = _
  var gradientF:Future[Boolean] = _

  def newInputVector(x: Array[Double]): Unit = {
    for (i <- 0 to x.length) {
      neurons(i) ! NewInputValue(x(i))
    }
  }

  def sendGradientArrayFuture(): Unit = {
    gradientP = Promise()
    gradientF = gradientP.future
    pipe(gradientF) to sender
  }

  def closeFuture():Unit = {
    gradientP.success(true)
    gradientCounter = 0
  }



  override def receive():Receive = {
    case NewInputVector(x) => newInputVector(x)
    case GradientArray() => sendGradientArrayFuture()
    case CountGradient => if (gradientCounter == units + 1) closeFuture() else gradientCounter = gradientCounter + 1
    case RegisterNextLayer(nextLayer) => registerNextLayer(nextLayer)
    case UpdateWeights(m, alpha) => updateWeights(m, alpha)
    case RegisterPreNeuron(neuron) => registerPreNeuron(neuron)
    case x => log.info("Unknown Message: " + x)
  }

  override def createNeurons(): Unit = {
    neurons(0) = context.actorOf(Props(new BiasNeuron), "BiasNeuron")
    for (i <- 1 to units) {
        neurons(i) = context.actorOf(Props(new InputNeuron()))
    }
  }

}
