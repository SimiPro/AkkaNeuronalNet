package simi.layer

import akka.actor.{Actor, ActorLogging, ActorRef}
import akka.util.Timeout
import simi.main._
import simi.neurons.Connection

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Promise, Future, Await}
import akka.pattern.ask
import akka.pattern.pipe

/**
 * Created by Simon on 19.04.2015.
 */
abstract class Layer(units:Int) extends Actor with ActorLogging {
  import context.dispatcher
  implicit val timeout = Timeout(5 seconds)


  var neurons:Array[ActorRef] = Array.ofDim[ActorRef](units)
  createNeurons()

  def createNeurons()

  def updateWeights(m: Int,alpha: Double): Unit = {
    neurons.foreach(N => {
      N ! UpdateWeights(m, alpha)
    })
  }

  def registerPreNeuron(preNeuron: ActorRef): Unit = {
    val connections = mutable.MutableList[Connection]()
    val connectionP = Promise[mutable.MutableList[Connection]]()

    // inputlayer <-- hiddenlayer
    connectionP.future pipeTo sender()

    for (i <- 0 until neurons.length) {
      //hiddenlayer --> BiasNeuron(InputNeuron)
      log.debug(this.getClass + " --> Neuron(" + neurons(i).getClass + ")")
      val future = neurons(i) ? RegisterPreNeuron(preNeuron)
      log.debug(this.getClass + "<-- Neuron(" + neurons(i).getClass + ")")
      future.map {
        case conn:Connection => {
          println(this.getClass + " Connection created")
          connections += conn
          if (connections.size == neurons.length) {
            connectionP.success(connections)
          }
        }
      }
    }
  }

  var gradientCounter = 0

  def onGradientFinished() = {
      log.debug("Gradient on: " + self.path + " Finished")
  }

  def increaseGradientCounter(): Unit = {
    gradientCounter = gradientCounter + 1
    if (gradientCounter == units) {
      onGradientFinished()
      gradientCounter = 0
    }
  }

  def receive = {
    case UpdateWeights(m, alpha) => updateWeights(m, alpha)
    case RegisterPreNeuron(neuron) => registerPreNeuron(neuron)
    case RegisterNextLayer(nextLayer) => registerNextLayer(nextLayer)
    case CountGradient() => increaseGradientCounter()
    case x => "Unknown Message: " + x
  }

  def claculateLayer(): Unit = {
      neurons.foreach(N => {
        N ! NewInputValue
      })
  }

  def registerNextNeuron(nextLayer: ActorRef, i:Int, resultP:Promise[Boolean]): Unit = {
    val N = neurons(i)
    log.debug("inputlayer  --> HiddenLayer(RegisterPreNeuron)")
    // inputlayer --> hiddenlayer(InputNeuron)
    val connectionsF = nextLayer ? RegisterPreNeuron(N)
    log.debug("hiddenlayer <-- Future, InputLayer")

    connectionsF onSuccess {
        case result:mutable.MutableList[Connection] => {
          if (i == neurons.length -1 ) {
            resultP.success(true)
          } else registerNextNeuron(nextLayer, i + 1, resultP)
        }
     }
  }

  def registerNextLayer(nextLayer: ActorRef): Unit = {
    val resultP = Promise[Boolean]()
    resultP.future pipeTo sender
    registerNextNeuron(nextLayer, 0,resultP)

    /*

    neurons.foreach(N => {
      val connections = nextLayer ? RegisterPreNeuron(N)
      connections onSuccess {
        case result:mutable.MutableList[Connection] => {
          N ! SetPostConnections(result)
          sender ! true
        }
        case x => {
          log.info("Something else here: " + x)
        }
      }
    })
  */

 }
}
