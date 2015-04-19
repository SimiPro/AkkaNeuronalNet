package simi.net

import akka.actor.{Props, ActorSystem}
import akka.testkit.{TestActorRef, ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import simi.main.{NewInputValue, SetPostConnections}
import simi.neurons.{Connection, HiddenNeuron, InputNeuron}

import scala.collection.mutable


class NetTest(_system: ActorSystem) extends TestKit(_system) with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("NetTest"))



  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "Create a Net " must {
    "test" in {
      var net = TestActorRef(Props(new Net(new NetBuilder())))
    }
  }
}