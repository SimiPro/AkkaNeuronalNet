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


  var gradientP:Promise[Boolean] = _
  var gradientF:Future[Boolean] = _

  def newInputVector(x: Array[Double]): Unit = {
    // trigger inputbiasneuron
    neurons(0) ! NewInputValue(1)
    for (i <- 0 until x.length) {
      neurons(i + 1) ! NewInputValue(x(i))
    }
  }

  def sendGradientArrayFuture(): Unit = {
    gradientP = Promise()
    gradientF = gradientP.future
    pipe(gradientF) to sender
  }


  override def onGradientFinished(): Unit = {
    super.onGradientFinished()
    closeFuture()
  }

  def closeFuture():Unit = {
    gradientP.success(true)
    gradientCounter = 0
  }



  override def receive():Receive = {
    case NewInputVector(x) => newInputVector(x)
    case CountGradient => increaseGradientCounter()
    case GradientArray => sendGradientArrayFuture()
    case RegisterNextLayer(nextLayer) => registerNextLayer(nextLayer)
    case UpdateWeights(m, alpha) => updateWeights(m, alpha)
    case RegisterPreNeuron(neuron) => registerPreNeuron(neuron)
    case x => log.info("Unknown Message: " + x)
  }

  override def createNeurons(): Unit = {
    neurons(0) = context.actorOf(Props(new BiasNeuron), "biasneuron")
    for (i <- 1 until units) {
        neurons(i) = context.actorOf(Props(new InputNeuron()), "inputneuron" + i)
    }
  }

}
