package com.yalingunayer.bastra.commons.domain

import java.util.Date


/**
 * A class that represents a game room.
 * This doesn't hold much information, but it's useful to abstract it away.
 */
case class GameRoom(val name: String) {
  val createdAt = new Date
}
