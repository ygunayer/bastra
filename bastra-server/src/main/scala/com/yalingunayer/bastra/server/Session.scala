package com.yalingunayer.bastra.server

import com.yalingunayer.bastra.commons.domain.GameRoom
import com.yalingunayer.bastra.commons.domain.Player
import akka.actor.ActorRef

/**
 * A class that represents a player's session
 */
case class PlayerSession(val player: Player, val ref: ActorRef)

/**
 * A class that represents a game session
 */
case class GameSession(val room: GameRoom, val ref: ActorRef, val players: List[PlayerSession])
