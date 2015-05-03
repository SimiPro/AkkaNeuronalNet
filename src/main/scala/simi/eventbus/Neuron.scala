package simi.eventbus

import akka.actor.{ActorPath, ActorRef, Actor}
import akka.actor.Actor.Receive
import akka.event.EventBus
import simi.neurons.NeuronState.NeuronState

/**
 * Created by simipro on 03/05/15.
 */
class Neuron extends EventBus with  {

  class NeuronState extends Enumeration {
    type NeuronState = Value
    val FORWARD, BACKWARD = Value
  }


  override type Classifier = this.type
  override type Subscriber = ActorRef // Mostly connections
  override type Event = NeuronImpuls

  /*
  If activationvalue is exeeded send out new value to all suscribers
   */
  def impuls(value: Double): Unit = {

  }



  override def subscribe(subscriber: Subscriber, to: Classifier): Boolean = ???

  override def publish(event: Event): Unit = ???



}

/**
 * This class is the abstraction between 2 Neurons.
 * Every Actor can register himself here to get the newValue*weight Impuls (Normally its just the Next Neuron)
 */
class Connection extends ActorRef {
  override def path: ActorPath = ???

  @deprecated("Use context.watch(actor) and receive Terminated(actor)")
  override def isTerminated: Boolean = ???
}


final case class ConnectionImpuls(activationValue:Double)

case class NeuronImpuls(state:NeuronState, value:Double)


