package io.simplifier.template.node

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}

class TemplateNode() extends Actor with ActorLogging {

  implicit val system: ActorSystem = context.system

  override def receive: Receive = {

    case msg =>
      log.debug(s"received $msg")

  }

}

object TemplateNode {

  val MODULE_NAME = "Template"
  val MODULE_GROUP_NAME = "template"
  val MAX_LENGTH_STRICT = 10000

  private var templateNode: Option[ActorRef] = None

  def start(system: ActorSystem): ActorRef = {
    templateNode.getOrElse({
      templateNode = Some(system.actorOf(props(), MODULE_NAME))
      templateNode.get
    })
  }

  def props(): Props =
    Props(new TemplateNode())

}
