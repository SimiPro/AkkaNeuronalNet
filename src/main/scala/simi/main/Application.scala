package simi.main


import akka.actor._
import akka.pattern.ask
import akka.pattern.pipe
import akka.util.Timeout
import simi.layer.Layer
import simi.neurons.{Connection, NeuronState, HiddenNeuron}
import NeuronState.NeuronState
import simi.mathli.MathHelper
import simi.net.{NetBuilder, Net}

import scala.collection.mutable
import scala.concurrent.{Promise, Future, Await}
import scala.concurrent.duration._
import scala.util.Random


object Application extends App {
  implicit val timeout = Timeout(5 seconds)

  var trainData = new mutable.MutableList[TrainSet]
  for (i <- 1 to 100) {
    trainData += TrainSet(Array[Double](0, 0), Array[Double](0))
  }
  for (i <- 1 to 100) {
    trainData += TrainSet(Array[Double](1, 0), Array[Double](0))
  }
  for (i <- 1 to 100) {
    trainData += TrainSet(Array[Double](0, 1), Array[Double](0))
  }
  for (i <- 1 to 100) {
    trainData += TrainSet(Array[Double](1, 1), Array[Double](1))
  }

  val system = ActorSystem("brain")

  // Exex context
  import system.dispatcher

  train()

  def train(): Unit = {
    val net = system.actorOf(Props(new Net(new NetBuilder(defaultHiddenLayers = false).addHiddenLayer(2))),"Brain" + Random.nextInt())
    println("Application --> Net(CreateNet)")
    val future = net ? CreateNet
    println("Application <-- Future, Net")
    future.onSuccess({
      case a => {
        println("Application,Future :::: Complete")
        train(trainData, net=net)
      }
    })
    future.onFailure({
      case e => println("OAAAA FAIIIL!!")
    })

  }

  def train(data:mutable.MutableList[TrainSet], i:Int = 1, net:ActorRef): Unit = {
    val costF = net.ask(Train(data))(5 seconds)
    costF onSuccess {
      case cost: Double => {
        println("Iteration: " + i + " | Cost: " + cost)
        if (cost > 0.09) {
          if(i > 1000) {
            println("New Try mby fall into a bad local minima")
            train()
          } else {
            train(data, i + 1, net)
          }
        }
      }
    }
  }
}


case class SetPostConnection(connection:Connection)
case class CreateNet()
case class Input(X:Array[Double])
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
case class BackProp( index:Int, cost:Double)
