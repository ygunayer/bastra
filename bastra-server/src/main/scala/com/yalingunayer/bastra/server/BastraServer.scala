package com.yalingunayer.bastra.server;

import akka.actor.ActorSystem
import akka.routing.FromConfig

object BastraServer {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("BastraServer")
    system.actorOf(LobbyActor.props(), "lobby")
  }
}
