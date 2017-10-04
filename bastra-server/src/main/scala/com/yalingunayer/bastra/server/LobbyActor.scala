package com.yalingunayer.bastra.server

import akka.actor.Actor
import akka.actor.Props
import com.yalingunayer.bastra.commons.domain.Player
import com.yalingunayer.bastra.commons.Messages
import akka.actor.ActorRef
import com.yalingunayer.bastra.commons.domain.GameRoom
import com.yalingunayer.bastra.commons.Utils
import akka.actor.Terminated

object LobbyActor {
  def props() = Props(classOf[LobbyActor])
}

class LobbyActor extends Actor {
  var games: Map[ActorRef, GameSession] = Map()
  var players: Map[ActorRef, PlayerSession] = Map()
  
  var waiting: List[PlayerSession] = List()
  
  def tryMakeMatch = waiting.length match {
    case x: Int if x > 1 => {
      val players = waiting.take(2)
      
      val room = new GameRoom(name = Utils.uuid)
      val roomRef = context.system.actorOf(GameRoomActor.props(room))
      
      waiting = waiting.drop(2)
      
      val p1 = players.head
      val p2 = players.tail.head
      
      println(f"Players ${p1} and ${p2} are matched and will promptly join the room ${room}")
      
      roomRef ! GameRoomActor.Messages.ReceivePlayers(p1, p2)
    }
    case _ => println("Not enough players to make a match")
  }
  
  def receive: Receive = {
    case Messages.Server.Connect(player: Player) => {
      println(f"Player connected: ${player}")
      
      sender ! Messages.Server.Connected()
      
      context.watch(sender)
      
      val session = PlayerSession(player, sender)
      players = players + (sender -> session)
      waiting = waiting :+ session
      tryMakeMatch
    }
    
    case Terminated(ref: ActorRef) if players.isDefinedAt(ref) => {
        // a player has disconnected
        val session = players.get(ref).get
        
        // remove them from the players list
        players = players - ref
        
        // and also from the waiting queue (if present)
        waiting = Utils.removeLast(waiting, session)
        
        println(f"Player has disconnected: ${session.player}")
    }
  }
  
  override def preStart() = {
    println("Server is now ready to accept connections")
  }
}
