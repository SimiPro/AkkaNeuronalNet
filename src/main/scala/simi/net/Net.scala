package simi.net

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import simi.layer.{OutputLayer, HiddenLayer, InputLayer}
import simi.main._

import scala.collection.mutable
import scala.concurrent.{Await, Future, Promise}
import akka.pattern.ask
import akka.pattern.pipe
import scala.concurrent.duration._

/**
 * Created by Simon on 19.04.2015.
 */
class Net(builder:NetBuilder) extends Actor with ActorLogging {
  import context.dispatcher
  implicit val timeout = Timeout(5 seconds)

  var inputLayer: ActorRef = _
  var hiddenLayers = new mutable.MutableList[ActorRef]
  var outputLayer: ActorRef = _
  var result: Promise[Array[Double]] = _
  var resultF: Future[Array[Double]] = _

  var costP :Promise[Double] = _
  var trainSet:mutable.MutableList[TrainSet] =_

  def connectNeurons() = {
      val created = Promise[Boolean]()
      created.future pipeTo sender()

      log.debug("Net --> InputLayer(RegisterNextLayer(hiddenLayers)")
      val future = inputLayer ? RegisterNextLayer(hiddenLayers(0))
      log.debug("Net <-- Future, InputLayer")

      future.map {
        case a => {
          println("Net, Future ::: InputLayer <--> 1. HiddenLayer Completed ")
          for (i <- 0 to hiddenLayers.length - 1) {
            println("Net --> HiddenLayer(RegisterNextLayer(Outputlayer)")
            val nextLayer = if (i == hiddenLayers.length - 1) outputLayer else hiddenLayers(i + 1)
            val futurez = hiddenLayers(i) ? RegisterNextLayer(nextLayer)
            futurez.map {
              // tell whom called CreateNet that we finished
              case x => {
                log.debug("Net, Future ::: 1.Hiddenlayer <--> Outputlayer Completed ")
                created.success(true)
              }
            }
          }
        }
      }

  }

  def createLayers() = {
    val bias = 1
    inputLayer = context.actorOf(Props(new InputLayer(builder.inputLayerUnits + bias)), builder.inputlayername)
    for (i <- 0 until builder.hiddenLayers.length) {
      val thisLayer = builder.hiddenLayers(i)
      hiddenLayers += context.actorOf(Props(new HiddenLayer(thisLayer.hiddenLayerUnits + bias)),thisLayer.name)
    }
    outputLayer = context.actorOf(Props(new OutputLayer(builder.outputLayerUnits)), "outputlayer")
  }

  def updateWeights(m:Int,alpha:Double = 0.3): Unit = {
    inputLayer ! UpdateWeights(m, alpha)
    hiddenLayers.foreach(L => {
      L ! UpdateWeights(m, alpha)
    })
  }

  def train(sets: mutable.MutableList[TrainSet]): Unit = {
    costP = Promise()
    pipe(costP.future) to sender()
    trainSet = sets

    backProp(0,0)
    updateWeights(sets.size)

  }

  def forwardProp(x:Array[Double]): Unit = {
    val resultFuture = outputLayer.ask(ResultArray())(5 seconds)
    inputLayer ! NewInputVector(x)

    resultFuture onSuccess {
      case result:Array[Double] => {
        sender ! result
      }
      case x => log.info("Something else: " + x)
    }
  }

  def backProp(i:Int,cost:Double):Unit = {
    val S = trainSet(i)
    log.debug("Hi Im the Backprop algo, Iteration: " + i + " TrainSet size: " + trainSet.size)
    val resultFuture = outputLayer.ask(ResultArray())(5 seconds)
    val gradientFuture = inputLayer ? GradientArray
    inputLayer ! NewInputVector(S.x)

    resultFuture onSuccess {
      case result:Array[Double] => {
        val newCost = cost + costFunction(result, S.y)
        outputLayer ! SetY(S.y)
        gradientFuture onSuccess {
          case y: Boolean => {
            if (i != trainSet.size - 1) {
              context.self ! BackProp(i + 1, newCost)
            } else {
              // log.info("COST: " + newCost)
              costP.success(newCost)
            }
          }
        }
      }
    }
  }

  def costFunction(a_L: Array[Double], y: Array[Double]): Double = {
    var cost = 0.0
    for (i <- 0 until a_L.size) {
      cost = cost + (-y(i)) * math.log(a_L(i)) - (1 - y(i)) * math.log(1 - a_L(i))  // + lambda1/2) * sum(allWeightsOfLayer1to2.^2) + lambda2/2) * sum(allWieghtsOfLayer2to3.^2)
    }
    cost
  }

  def input(x: Array[Double]): Unit = {
    forwardProp(x)
  }

  /**
   * Returns a future. As soon as its finished, u can start use the net
   */
  def createNet(): Unit = {


    createLayers()
    connectNeurons()

  }


  override def receive: Receive = {
    case Train(data) => train(data)
    case CreateNet => createNet()
    case Input(x) => input(x)
    case BackProp(index, cost) => backProp(index, cost)
    case x => log.info("Unknown Message: " + x)
  }
}
