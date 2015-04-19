package simi.layer

import akka.actor.Props
import simi.main._
import simi.neurons.HiddenNeuron
import akka.pattern.pipe

import scala.concurrent.{Future, Promise}

/**
 * Created by Simon on 19.04.2015.
 */
class OutputLayer(units:Int) extends Layer(units) {
  import context.dispatcher

  var result:Array[Double] = Array.ofDim[Double](units)
  var resultCounter = 0
  var resultP:Promise[Array[Double]] = _
  var resultF:Future[Array[Double]] = _

  def getResultFuture(): Unit = {
    resultP = Promise()
    resultF = resultP.future
    pipe(resultF) to sender
  }

  def setY(y: Array[Double]): Unit = {
    for(i <- 0 until y.length) neurons(i) ! Y(y(i))
  }

  def addToResult(a: Double, index: Int): Unit = {
    result(index) = a
    if (resultCounter == units) {
      resultP.success(this.result)
      resultCounter = 0
    }else {
      resultCounter = resultCounter + 1
    }
  }

  override def receive:Receive = {
    case ResultArray() => getResultFuture()
    case SetY(y) => setY(y)
    case UpdateWeights(m, alpha) => updateWeights(m, alpha)
    case RegisterPreNeuron(neuron) => registerPreNeuron(neuron)
    case SendResult(a, index) => addToResult(a,index)
    case x => "Unknown Message: " + x
  }

  override def createNeurons(): Unit = {
    for (i <- 1 until units) {
      neurons(i) = context.actorOf(Props(new HiddenNeuron), "hiddenneuron" + i)
    }
  }
  }
