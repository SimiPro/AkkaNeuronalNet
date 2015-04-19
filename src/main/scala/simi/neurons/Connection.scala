package simi.neurons

import akka.actor.{ActorRef, ActorSelection}
import simi.mathli.MathHelper

/**
 * Created by Simon on 19.04.2015.
 */
case class Connection(var weight:Double = MathHelper.randomInitializedNumber(), nextNeuron:ActorRef, prevNeuron:ActorRef)
