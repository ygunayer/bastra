package com.yalingunayer.bastra.commons.domain

/**
 * Holds general information about a player.
 */
case class Player(id: String, name: String)

/**
 * Represents the score a player has in a game.
 */
case class PlayerScore(totalPoints: Int, bastras: Int, fished: Int)

object PlayerScore {
  val zero = PlayerScore(0, 0, 0)
}

/**
 * Represents the state the player is currently in. Intended only for the player themselves to see.
 */
case class PlayerState(hand: CardStack, bucket: CardStack, score: PlayerScore)
