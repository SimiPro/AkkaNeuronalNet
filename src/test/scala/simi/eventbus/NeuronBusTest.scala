package simi.eventbus

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import org.scalatest.concurrent.ScalaFutures

/**
 * Created by simipro on 03/05/15.
 */
class NeuronBusTest(_system: ActorSystem) extends TestKit(_system)
with ScalaFutures
with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll {


  def this() = this(
    ActorSystem("NetTest")
  )

  "NeuronBus" must {
      ""

  }
}
