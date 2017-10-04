package com.yalingunayer.bastra.server

import com.yalingunayer.bastra.commons.domain.GameRoom
import akka.actor.Props
import akka.actor.Actor
import com.yalingunayer.bastra.commons.Messages
import com.yalingunayer.bastra.commons.domain.CardStack
import akka.actor.ActorRef
import akka.actor.PoisonPill
import akka.remote.DisassociatedEvent
import akka.actor.Terminated
import scala.util.Random
import com.yalingunayer.bastra.commons.domain.Card
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import com.yalingunayer.bastra.commons.domain.PlayerScore
import com.yalingunayer.bastra.commons.domain.PlayerState

object GameRoomActor {
  case class PlayerInformation(session: PlayerSession, state: PlayerState)
  case class GameState(isFinished: Boolean, deck: CardStack, middleStack: CardStack, playerInTurn: PlayerInformation, playerWaiting: PlayerInformation)
  case class StateResult(newState: GameRoomActor.GameState, winner: Option[PlayerInformation], isFished: Boolean)
  
  object Messages {
    case class ReceivePlayers(p1: PlayerSession, p2: PlayerSession)
  }
  
  def props(room: GameRoom) = Props(classOf[GameRoomActor], room)
  
  /**
   * Determine the next state based on a card that was played
   */
  def determineNextState(card: Card, playerInfo: PlayerInformation, opponentInfo: PlayerInformation, deckCards: List[Card], middleCards: List[Card]): StateResult = {
    val player = playerInfo.state
    val opponent = opponentInfo.state
    
    // determine if the player has fished a card, what their scores will be, and what their bucket will contain on the next turn
    // since this will also affect the middle stack, determine its new state as well
    val (isFished, newScore, newMiddleStack, newBucketStack) = middleCards.headOption match {
      case Some(other) if card.canFish(other) => {
        val newBucketCards = card :: middleCards ++ player.bucket.cards
        
        // find out if a bastra is performed
        val isBastra = middleCards match {
          case x :: Nil if (x == card) => true
          case _ => false
        }
        
        val earnedPoints = (card :: middleCards).map(_.score).sum + (if (isBastra) 10 else 0)
        
        val newScore = PlayerScore(earnedPoints + player.score.totalPoints, player.score.bastras + (if (isBastra) 1 else 0), newBucketCards.length)
        
        (true, newScore, CardStack.empty, CardStack(newBucketCards))
      }
      case _ => (false, player.score, CardStack(card :: middleCards), player.bucket)
    }
    
    val proposedNextHand = player.hand.removed(card).cards
    val bothHandsEmpty = proposedNextHand.isEmpty && opponent.hand.isEmpty
    val isFinished = bothHandsEmpty && deckCards.isEmpty
    val shouldDeal = !isFinished && bothHandsEmpty
    
    // determine if we need to deal new cards to players
    val (nextDeck, nextPlayerHand, nextOpponentHand) = shouldDeal match {
      case true => (deckCards.drop(8), deckCards.take(4), deckCards.drop(4).take(4))
      case _ => (deckCards, proposedNextHand, opponent.hand.cards)
    }
    
    val nextPlayerState = PlayerState(CardStack(nextPlayerHand), newBucketStack, newScore)
    val nextPlayerInfo = PlayerInformation(playerInfo.session, nextPlayerState)
    
    val nextOpponentState = PlayerState(CardStack(nextOpponentHand), opponent.bucket, opponent.score)
    val nextOpponentInfo = PlayerInformation(opponentInfo.session, nextOpponentState)
    
    val nextState = GameState(isFinished, CardStack(nextDeck), newMiddleStack, nextOpponentInfo, nextPlayerInfo)
    
    // determine the winner (if any)
    val winner: Option[PlayerInformation] = isFinished match {
      case true => (nextPlayerInfo :: nextOpponentInfo :: Nil).sortBy(_.state.score.totalPoints).tail.headOption
      case _ => None
    }
    
    StateResult(nextState, winner, isFished)
  }
}

class GameRoomActor(val room: GameRoom) extends Actor {
  var players: Map[ActorRef, PlayerSession] = Map()
  
  /**
   * Start a new game with the specified players.
   */
  def startGame(p1: PlayerSession, p2: PlayerSession) = {
    p1.ref ! Messages.Game.Joined(room)
    p2.ref ! Messages.Game.Joined(room)
    
    val deck = CardStack.shuffled.cards
    
    val hand1 = deck.take(4)
    val hand2 = deck.drop(4).take(4)
    val mid = deck.drop(8).take(4)
    val remainder = deck.drop(12)
    
    val first = GameRoomActor.PlayerInformation(p1, PlayerState(hand1, CardStack.empty, PlayerScore.zero))
    val second = GameRoomActor.PlayerInformation(p2, PlayerState(hand2, CardStack.empty, PlayerScore.zero))
    
    val initialState = GameRoomActor.GameState(false, remainder, mid, first, second)
    
    context.become(playing(initialState))
    
    println(f"Starting a game between ${first.state} and ${second.state}. Other stacks are as follows: ${mid} (middle), ${remainder} (deck)")
    
    first.session.ref ! Messages.Game.SetUp(second.session.player)
    first.session.ref ! Messages.Game.InTurn(mid.headOption, false, first.state, second.state.score)
    
    second.session.ref ! Messages.Game.SetUp(first.session.player)
    second.session.ref ! Messages.Game.OpponentInTurn(mid.headOption, false, second.state, first.state.score)
  }
  
  /**
   * Determine the next state of the game given its current state and a card that was played
   */
  def getNextState(player: ActorRef, card: Card, current: GameRoomActor.GameState): Try[GameRoomActor.StateResult] = {
    if (current.playerInTurn.session.ref != player) {
      Failure(new Exception("Please wait until it's your turn."))
    } else if (!current.playerInTurn.state.hand.cards.contains(card)) {
      Failure(new Exception("You have played an invalid card."))
    } else {
      val player = current.playerInTurn
      val opponent = current.playerWaiting
      val deckCards = current.deck.cards
      val middleCards = current.middleStack.cards 
      
      val result = GameRoomActor.determineNextState(card, player, opponent, deckCards, middleCards)
      
      Success(result)
    }
  }
  
  /**
   * A player has left. Terminate the game.
   */
  def onPlayerLeft(ref: ActorRef): Unit = {
    val session = players.get(ref).get
    println(f"Player ${session.player} has left the game.")
    self ! Messages.Game.Terminate(session.player)
  }
  
  /**
   * The inital state. Wait for the matchmaking server to introduce the players.
   */
  def initializing: Receive = {
    case GameRoomActor.Messages.ReceivePlayers(p1: PlayerSession, p2: PlayerSession) => {
      players = Map() + (p1.ref -> p1) + (p2.ref -> p2)
      
      context.watch(p1.ref)
      context.watch(p2.ref)
      
      startGame(p1, p2)
    }
  }
  
  /**
   * The game is being played.
   */
  def playing(state: GameRoomActor.GameState): Receive = {
    case t: Messages.Game.Terminate => {
      println(f"Terminating the game ${room.name} due to ${t.reason}")
      
      state.playerInTurn.session.ref ! t
      state.playerWaiting.session.ref ! t
      
      self ! PoisonPill
    }
    
    case Terminated(ref: ActorRef) if players.isDefinedAt(ref) => onPlayerLeft(ref)
    case Messages.Game.Leave() if players.isDefinedAt(sender) => onPlayerLeft(sender)
    
    case Messages.Game.CardPlayed(card) if players.isDefinedAt(sender) => {
      println(f"Player ${sender} played ${card}")
      getNextState(sender, card, state) match {
        case Success(GameRoomActor.StateResult(state, winner, isFished)) => {
          winner match {
            case Some(player) => {
              val opponent = players.values.filter(_.ref != player.session.ref).head
              println(f"Game finished, winner is ${player}")
              
              player.session.ref ! Messages.Game.Win()
              opponent.ref ! Messages.Game.Lose()
              
              context.become(finished(players.keys.toSet))
            }
            case _ => {
              val nextPlayer = state.playerInTurn
              val previousPlayer = state.playerWaiting
              
              println(f"Card accepted, moving to new turn. Player in turn: ${nextPlayer.state}, opponent: ${previousPlayer.state}, game state: ${state}")
              
              nextPlayer.session.ref ! Messages.Game.InTurn(state.middleStack.cards.headOption, isFished, nextPlayer.state, previousPlayer.state.score)
              previousPlayer.session.ref ! Messages.Game.OpponentInTurn(state.middleStack.cards.headOption, isFished, previousPlayer.state, nextPlayer.state.score)
              
              context.become(playing(state))
            }
          }
        }
        case Failure(e) => sender ! Messages.Server.Message(e.getMessage())
      }
    }
  }
  
  /**
   * The game is finished. Wait for rematch responses.
   */
  def finished(pending: Set[ActorRef]): Receive = {
    case Messages.Player.Accept() if players.isDefinedAt(sender) => {
      val newPending = pending - sender
      val player = players(sender)
      println(f"Player ${player.player} has agreed to rematch")
      
      if (newPending.isEmpty) {
        println("Restarting game...")
        val sessions = players.values
        
        sessions.foreach(player => player.ref ! Messages.Game.Restart)
        
        startGame(sessions.head, sessions.tail.head)
      } else {
        println(f"Pending ${newPending.size} more replies")
        context.become(finished(newPending))
      }
    }
    
    case Messages.Player.Refuse() if players.isDefinedAt(sender) => {
      val player = players(sender)
      println(f"Player ${player.player} has refused to rematch")
      onPlayerLeft(sender)
    }
  }
  
  def receive = initializing
}
