package simi.neurons

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestActorRef, ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}


import scala.collection.mutable

class NeuronTest(_system: ActorSystem) extends TestKit(_system) with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("NetTest"))



  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "A Neuron " must {

    "test" in {
      val inputNeuron = TestActorRef(Props(new InputNeuron))
      val hiddenNeruon = TestActorRef(Props(new HiddenNeuron))

      val hiddenInstance = hiddenNeruon.underlyingActor.asInstanceOf[HiddenNeuron]
      val neuronInstance = inputNeuron.underlyingActor.asInstanceOf[InputNeuron]
      //inputNeuron ! SetPostConnections(mutable.MutableList[Connection](Connection(nextNeuron = hiddenNeruon, prevNeuron = inputNeuron)))
      assert(true)
    }

    "increase activation value on NewInputValue and resets if exeeded" in {
      val inputNeuron = TestActorRef(Props(new InputNeuron))
      val hiddenNeruon = TestActorRef(Props(new HiddenNeuron))

      val neuronInstance = inputNeuron.underlyingActor.asInstanceOf[InputNeuron]
   //   inputNeuron ! SetPostConnections(mutable.MutableList[Connection](Connection(nextNeuron = hiddenNeruon, prevNeuron = inputNeuron)))

      neuronInstance.activationValue = 2
     // inputNeuron ! NewInputValue(1)
      assert(neuronInstance.currentActivationValue == 1)
    //  inputNeuron ! NewInputValue(1)
      assert(neuronInstance.currentActivationValue == 0)
    }
  }
}