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

  createLayers()
  connectNeurons()



  def connectNeurons() = {
      val future = inputLayer ? RegisterNextLayer(hiddenLayers(0))
      future onSuccess {
        case a => {
          for (i <- 0 until hiddenLayers.length) {
            val nextLayer = if (i == hiddenLayers.length - 1) outputLayer else hiddenLayers(i + 1)
            val future =  hiddenLayers(i) ? RegisterNextLayer(nextLayer)
            Await.ready(future, 5 seconds)
          }
        }
      }







  /*
     outputLayer ? RegisterPrevLayer(hiddenLayers(hiddenLayers.length - 1))

     for (i <- hiddenLayers.length - 1 to 0) {
       val prevLayer = if (i == 0) inputLayer else hiddenLayers(i - 1)
       hiddenLayers(i) ? RegisterPrevLayer(prevLayer)
     }
     */
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
    outputLayer ! UpdateWeights(m, alpha)
  }

  def train(sets: mutable.MutableList[TrainSet]): Unit = {
    result = Promise()
    resultF = result.future
    pipe(resultF) to sender()

    backProp(sets, 0)
    updateWeights(sets.size)

  }

  def backProp(sets: mutable.MutableList[TrainSet], i:Int):Unit = {
    val S = sets(i)
    val resultFuture = outputLayer.ask(ResultArray())(5 seconds)
    inputLayer ! NewInputVector(S.x)

    resultFuture onSuccess {
      case result:Array[Double] => {
        val gradient:Future[Boolean] = inputLayer.ask(GradientArray)(2 seconds).mapTo[Boolean]
        outputLayer ! SetY(S.y)
        gradient onSuccess {
          case y: Boolean => {
            if (i != sets.size - 1) backProp(sets, i + 1)
          }
        }
      }
    }
  }

  override def receive: Receive = {
    case Train(data) => train(data)

    case x => log.info("Unknown Message: " + x)
  }
}
