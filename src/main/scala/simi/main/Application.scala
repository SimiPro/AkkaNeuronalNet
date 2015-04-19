package simi.main


import akka.actor._
import akka.pattern.ask
import akka.pattern.pipe
import simi.layer.Layer
import simi.neurons.{Connection, NeuronState, HiddenNeuron}
import NeuronState.NeuronState
import simi.mathli.MathHelper
import simi.net.{NetBuilder, Net}

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





  def train(data:mutable.MutableList[TrainSet], i:Int = 0): Unit = {
    val costF = net.ask(Train(data))(1 seconds)
    costF onSuccess {
      case cost: Double => {
        println("Iteration: %i | Cost: %d",i,cost)
        if (cost > 0.9) train(data)
      }
    }
  }
}




case class SetPostConnections(postConnections:mutable.MutableList[Connection])
case class RegisterPostNeuron(postNeuron:ActorRef)
case class RegisterPreNeuron(preNeuron:ActorRef)
case class RegisterNextLayer(nextLayer:ActorRef)
case class RegisterPrevLayer(prevLayer:ActorRef)
case class Y(y:Double)
case class CountGradient()
case class Train(trainSet: mutable.MutableList[TrainSet])
case class TrainSet(x:Array[Double], y:Array[Double])
case class UpdateWeights( m:Int,alpha:Double)
case class SetY(y:Array[Double])
case class ResultArray()
case class GradientArray()
case class SendResult(a:Double, index:Int)
case class DeltaImpuls(weight:Double, delta:Double)
case class NewInputValue(value:Double)
case class NewInputVector(x:Array[Double])
