package simi.main


import akka.actor._
import akka.pattern.ask
import akka.pattern.pipe
import simi.main.NeuronState.NeuronState
import simi.mathli.MathHelper

import scala.collection.mutable
import scala.concurrent.{Promise, Future, Await}
import scala.concurrent.duration._



object Application extends App {
  var trainData = new mutable.MutableList[TrainSet]
  for (i <- 1 to 5) {
    trainData += TrainSet(Array[Double](0, 0), Array[Double](0))
  }
  for (i <- 1 to 5) {
    trainData += TrainSet(Array[Double](1, 0), Array[Double](0))
  }
  for (i <- 1 to 5) {
    trainData += TrainSet(Array[Double](0, 1), Array[Double](0))
  }
  for (i <- 1 to 5) {
    trainData += TrainSet(Array[Double](1, 1), Array[Double](1))
  }




  val system = ActorSystem("brain")

  // Exex context
  import system.dispatcher

  var net = system.actorOf(Props(new Net(new NetBuilder())))
  train(trainData)





  def train(data:mutable.MutableList[TrainSet]): Unit = {
    val costF = net.ask(Train(data))(1 seconds)
    costF onSuccess {
      case cost:Double => if (cost > 0.9) train(data)
    }
  }


}

case class Train(trainSet: mutable.MutableList[TrainSet])

case class TrainSet(x:Array[Double], y:Array[Double])

class NetBuilder(var inputLayerUnits:Int = 2, var outputLayerUnits:Int = 1, var defaultHiddenLayers:Boolean = true) {
  var hiddenLayers = mutable.MutableList[HiddenLayerBuilder]()

  def createHiddenLayers() = {
    if (defaultHiddenLayers) addHiddenLayer()
  }

  createHiddenLayers()

  def addHiddenLayer(hiddenLayerUnits:Int = 1): Unit = {
    hiddenLayers += new HiddenLayerBuilder(hiddenLayerUnits)
  }

  class HiddenLayerBuilder(var hiddenLayerUnits:Int = 1) {

  }

}
case class UpdateWeights(alpha:Float, m:Int)

class Net(builder:NetBuilder) extends Actor with ActorLogging {
  import context.dispatcher
  createLayers()
  var inputLayer:ActorRef = _
  var hiddenLayers:mutable.MutableList[ActorRef] = mutable.MutableList()
  var outputLayer:ActorRef = _
  var result:Promise[Array[Double]] = _
  var resultF:Future[Array[Double]] = _


  def createLayers() = {
    inputLayer = context.actorOf(Props(new InputLayer(builder.inputLayerUnits)))
    builder.hiddenLayers.foreach(L => {
    hiddenLayers +=  context.actorOf(Props(new HiddenLayer(L.hiddenLayerUnits)))
    })
    outputLayer = context.actorOf(Props(new OutputLayer(builder.outputLayerUnits)))
  }

  def updateWeights(m:Int,alpha:Float = 0.3): Unit = {
    inputLayer ! UpdateWeights(alpha, m)
    hiddenLayers.foreach(L => {
      L ! UpdateWeights(alpha, m)
    })
    outputLayer ! UpdateWeights(alpha, m)
  }

  def train(sets: mutable.MutableList[TrainSet]): Unit = {
    result = Promise()
    resultF = result.future
    pipe(resultF) to sender()

    backProp(sets, 0)
    updateWeights()






  }

  def backProp(sets: mutable.MutableList[TrainSet], i:Int):Unit = {
    val S = sets(i)
    val resultFuture = outputLayer.ask(ResultArray())(5 seconds)
    inputLayer ! NewInputVector(S.x)

    resultFuture onSuccess {
      case result:Array[Double] => {
        val gradient:Future[Boolean] = inputLayer.ask(GradientArray).mapTo[Boolean]
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
case class SetY(y:Array[Double])
case class ResultArray()

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
    case x => log.info("Unknown Message: " + x)
  }

}

case class GradientArray()

class HiddenLayer(units:Int) extends Layer(units) {

}
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
    case SendResult(a, index) => addToResult(a,index)
  }

}

case class Y(y:Double)



abstract class Layer(units:Int) extends Actor with ActorLogging {
  var neurons:Array[ActorRef] = Array.ofDim[ActorRef](units)

  def updateWeights(fl: Float, i: Int): Unit = ???

  def receive = {
    case UpdateWeights(alpha, m) => updateWeights(alpha, m)
  }
  
  def claculateLayer(): Unit = {
      neurons.foreach(N => {
        N ! NewInputValue
      })
  }
}

case class CalculateLayer()

object NeuronState extends Enumeration {
  type NeuronState = Value
  val exeeded, tolow = Value
}

abstract class Neuron(unitsOfNextLayer:Int, unitsOfPrevLayer:Int) extends Actor with ActorLogging {
  val weights = Array.ofDim[Connection](unitsOfNextLayer)
  var activationValue = unitsOfPrevLayer
  var currentActivationValue = 0
  var z_value = 0.0
  var a_value = 0.0
  var gradient = 0.0
  def increaseCurrentActivationValue() = currentActivationValue = currentActivationValue + 1
  def addValue(value:Double) = z_value  = z_value + value


  def receive = {
    case NewInputValue(value) => newInputValue(value)
    case Y(y) => weights.foreach(N => N.prevNeuron ! DeltaImpuls(N.weight,a_value - y))
    case DeltaImpuls(weight, delta) => calculateDeltaAndWeight(weight, delta)
    case x => log.info("Unknown Message: " + x)
  }

  def newInputValue(value: Double): Unit = {
    increaseCurrentActivationValue()
    addValue(value)
    if (currentActivationValue == activationValue) {
      a_value = activationFunction()
      trigger()
      z_value = 0
    }
  }


  def calculateDeltaAndWeight(weight:Double, delta:Double) = {
    gradient = gradient +  delta*a_value
    val newDelta = weight*delta *(a_value*(1-a_value))

    if (weights(0).prevNeuron != null) {
      weights.foreach(C => {
        C.prevNeuron ! DeltaImpuls(weight, newDelta)
      })
    }
    context.parent ! CountGradient()
  }

  /**
   * return a_value
   */
  def activationFunction():Double
  def trigger()

}

case class CountGradient()

class InputNeuron(unitsOfNextLayer:Int,unitsOnPrevLayer:Int) extends  Neuron(unitsOfNextLayer, unitsOnPrevLayer) with ActorLogging {
  override def trigger(): Unit = {
    weights.foreach(C => {
      C.nextNeuron ! a_value*C.weight
    })
  }

  override def activationFunction(): Double = {
    z_value
  }
}

class HiddenNeuron(unitsOfNextLayer:Int,unitsOnPrevLayer:Int) extends  Neuron(unitsOfNextLayer, unitsOnPrevLayer) with ActorLogging {
  override def trigger(): Unit = {
    weights.foreach(C => {
      C.nextNeuron ! a_value*C.weight
    })
  }

  override def activationFunction(): Double = {
    MathHelper.sigmoid(z_value)
  }


}
class OutputNeuron(unitsOfNextLayer:Int,unitsOnPrevLayer:Int, index:Int) extends  Neuron(unitsOfNextLayer, unitsOnPrevLayer) with ActorLogging {
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


case class SendResult(a:Double, index:Int)
case class Connection(weight:Double, nextNeuron:ActorRef, prevNeuron:ActorRef)
case class DeltaImpuls(weight:Double, delta:Double)
case class NewInputValue(value:Double)
case class NewInputVector(x:Array[Double])


