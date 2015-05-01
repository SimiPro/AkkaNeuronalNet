package simi.net

import java.util.Properties

import akka.actor.{Props, ActorSystem}
import akka.testkit.{TestActorRef, ImplicitSender, TestKit}
import akka.util.Timeout
import com.typesafe.config.{ConfigIncluder, ConfigParseOptions, ConfigFactory}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Millis, Span}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import simi.main.{CreateNet, Input, NewInputValue}
import simi.neurons.{Connection, HiddenNeuron, InputNeuron}
import akka.pattern.ask
import collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent.duration._


import scala.concurrent.duration._

import scala.util.Success

class NetTest(_system: ActorSystem) extends TestKit(_system)
with ScalaFutures
with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll {
  implicit val timeout = Timeout(10 seconds)
  import this._system.dispatcher
  val props = new Properties()
  props.put("akka.actor.debug.receive", "on")
  props.put("akka.loglevel", "DEBUG")
  var config = ConfigFactory.parseProperties(props)

  def this() = this(
    ActorSystem("NetTest")
  )



  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "Net " must {
    "Create a Net" in {
      /*
      val net = TestActorRef(Props(new Net(new NetBuilder())), "Net1")


      val future = net ? CreateNet
      future onSuccess {
        case x:Boolean => assert(x)
      }
      */
    }

    "Forward Prop And" in {
      val net = system.actorOf(Props(new Net(new NetBuilder())), "Net2")

      val future = net ? CreateNet

      whenReady (future, timeout(Span(20, Seconds)), interval(Span(150, Millis))) {
        r =>{
          assert(r.asInstanceOf[Boolean])
          val x = Array[Double](0,0)
          val result = net ? Input(x)
          val Success(resultz:Array[Double]) = result.value.get
          resultz(0) should be (1)
        }
      }
    }
  }
}