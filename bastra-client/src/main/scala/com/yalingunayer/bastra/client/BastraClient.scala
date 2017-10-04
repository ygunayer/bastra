
package com.yalingunayer.bastra.client;

import akka.actor.ActorSystem

object BastraClient {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("BastraClient")
    system.actorOf(PlayerActor.props)
  }
}
