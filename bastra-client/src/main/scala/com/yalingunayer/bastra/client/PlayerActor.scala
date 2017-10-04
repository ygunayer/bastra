package com.yalingunayer.bastra.client

import akka.actor.Actor
import com.yalingunayer.bastra.commons.domain.Player
import com.yalingunayer.bastra.commons.Utils
import scala.util.Success
import akka.actor.PoisonPill
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import akka.actor.ActorRef
import scala.util.Failure
import akka.actor.Props
import com.yalingunayer.bastra.commons.Messages
import com.yalingunayer.bastra.commons.domain.GameRoom
import com.yalingunayer.bastra.commons.domain.Card
import com.yalingunayer.bastra.commons.domain.CardStack
import com.yalingunayer.bastra.commons.domain.PlayerState
import com.yalingunayer.bastra.commons.domain.PlayerScore

object PlayerActor {
  def props: Props = Props(classOf[PlayerActor])
}

class PlayerActor extends Actor {
  private object Events {
    case class ServerFound(ref: ActorRef)
    case class CardPlayed(card: Card)
  }

  /**
   * The currently joined server
   */
  var server: ActorRef = null

  /**
   * The currently joined game room
   */
  var room: GameRoom = null

  /**
   * Actor reference to the currently joined game room
   */
  var roomRef: ActorRef = null

  /**
   * The player
   */
  var me: Player = null

  /**
   * The opponent
   */
  var opponent: Player = null

  /**
   * The player's hand
   */
  var hand: CardStack = null

  /**
   * Reset the game state. This is usually called before restarting a new game.
   */
  def resetState: Unit = {
    room = null
    opponent = null
    hand = null
  }

  /**
   * Leave the room and reset the game state
   */
  def leaveRoom: Unit = {
    roomRef ! Messages.Game.Leave()

    resetState
    room = null
    roomRef = null

    context.become(waiting)
  }

  /**
   * Print the current state of the game and prompt the user to play a card
   */
  def promptCard(top: Option[Card]): Unit = {
    def doPrompt: Unit = {
      Utils.readNumericResponse.onComplete {
        case Success(idx: Some[Int]) if hand.cards.isDefinedAt(idx.get - 1) => {
          val card = hand.cards(idx.get - 1)
          self ! Events.CardPlayed(card)
        }
        case _ => {
          println("Invalid input, please try again.")
          doPrompt
        }
      }
    }

    val topCardName = top match { case Some(c: Card) => c.toString case _ => "None" }

    println(f"The top-most card is $topCardName")

    println(f"Please pick a card to play:")

    hand.cards.zipWithIndex.foreach { case (card, idx) => println(f"${(idx + 1)}. $card") }

    doPrompt
  }

  /**
   * Ask if the player wants a rematch
   */
  def promptRematch = {
    def doPrompt: Unit = {
      Utils.readBooleanResponse.onComplete {
        case Success(result) => {
          result match {
            case true => {
              println("Waiting for the opponent's answer")
              
              roomRef ! Messages.Player.Accept()
            }
            case false => {
              println("Leaving the game")
              
              roomRef ! Messages.Player.Refuse()

              leaveRoom
            }
          }
        }
        case _ => {
          println("Invalid input, please try again.")
          doPrompt
        }
      }
    }

    println(f"Would you like a rematch?")
    doPrompt
  }

  /**
   * The player is waiting to connect to a game lobby
   */
  def connecting: Receive = {
    case m: Messages.Server.Connected => {
      println(f"Connected to the server, waiting for opponents...")
      context.become(waiting)
    }
  }

  /**
   * The player is now in the lobby, and waiting to join a game room
   */
  def waiting: Receive = {
    case m: Messages.Game.Joined => {
      room = m.room
      roomRef = sender

      println(f"You have joined a game room: ${room.name}")
      context.become(waitingToStart)
    }
  }

  /**
   * The player has joined a game room and is now waiting for the game to start
   */
  def waitingToStart: Receive = {
    case m: Messages.Game.SetUp => {
      opponent = m.opponent

      println(f"Your are playing with ${opponent.name}")
      context.become(turnPending)
    }
  }

  /**
   * An abstract state that covers the part where the player is playing the game, whether it's their turn or their opponent's
   */
  def playing: Receive = {
    case Messages.Game.Lose() => {
      println("You have lost the game.")

      context.become(finished)

      promptRematch
    }
    case Messages.Game.Win() => {
      println("You have won the game!")

      context.become(finished)

      promptRematch
    }
    case t: Messages.Game.Terminate => t.reason match {
      case reason: Messages.Game.Terminate.Reason.PlayerLeft if reason.player != me => {
        println(f"Your opponent has left the game, you will be returned to the lobby")
        leaveRoom
      }
      case reason: Messages.Game.Terminate.Reason.ErrorOccurred => {
        println(f"The game was terminated due to an error, you will be returned to the lobby. Reason was: ${reason.error}")
        leaveRoom
      }
    }
    case Events.CardPlayed => println("Please wait until it's your turn")
    case Messages.Server.Message(msg) => println(f"[Server]: $msg")
  }

  /**
   * The "limbo" state where the player is waiting for the server to decide whose turn it is
   */
  def turnPending: Receive = playing orElse {
    case Messages.Game.InTurn(top: Option[Card], isFished: Boolean, state: PlayerState, opponentScore: PlayerScore) => {
      hand = state.hand
      
      if (isFished) {
        print("The opponent has successfully fished cards. ")
      }

      println(f"It's your turn now. Your Score: ${state.score}, Opponent's Score: ${opponentScore}")
      
      context.become(inTurn)

      promptCard(top)
    }
    case Messages.Game.OpponentInTurn(top: Option[Card], isFished: Boolean, state: PlayerState, opponentScore: PlayerScore) => {
      println(f"It's the opponent's turn now. Your Score: ${state.score}, Opponent's Score: ${opponentScore}")
      
      context.become(opponentInTurn)
    }
  }

  /**
   * It's the opponent's turn
   */
  def opponentInTurn: Receive = turnPending

  /**
   * It's the player's turn
   */
  def inTurn: Receive = turnPending orElse {
    case Events.CardPlayed(card) => {
      roomRef ! Messages.Game.CardPlayed(card)

      println(f"You have played $card")
    }
  }

  /**
   * The game is finished. Wait for a possible restart
   */
  def finished: Receive = playing orElse {
    case Messages.Game.Restart => {
      println("Opponent accepted rematch, starting a new round")
      resetState
      context.become(waitingToStart)
    }
  }

  /**
   * The default state is the connecting state
   */
  def receive = connecting

  /**
   * Try reconnecting until successful
   */
  def tryReconnect = {
    def doTry(attempts: Int): Unit = {
      context.system.actorSelection("akka.tcp://BastraServer@127.0.0.1:47000/user/lobby").resolveOne()(10.seconds).onComplete(x => x match {
        case Success(ref: ActorRef) => {
          println("Server found, attempting to connect...")
          server = ref
          server ! Messages.Server.Connect(me)
        }
        case Failure(t) => {
          System.err.println(f"No game server found, retrying (${attempts + 1})...")
          Thread.sleep(5000) // this is almost always a bad idea, but let's keep it for simplicity's sake
          doTry(attempts + 1)
        }
      })
    }

    println("Attempting to find a game server...")
    context.become(connecting)
    doTry(0)
  }

  override def preStart(): Unit = {
    println("Welcome to Bastra! Please enter your name.")
    Utils.readResponse.onComplete {
      case Success(name: String) => {
        me = Player(Utils.uuid(), name)
        tryReconnect
      }

      case _ => self ! PoisonPill
    }
  }
}
