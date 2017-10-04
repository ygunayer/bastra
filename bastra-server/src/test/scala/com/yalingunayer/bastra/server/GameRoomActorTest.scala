package com.yalingunayer.bastra.server;

import org.scalatest._
import com.yalingunayer.bastra.commons.domain.CardStack
import com.yalingunayer.bastra.commons.domain.Card
import com.yalingunayer.bastra.commons.domain.Rank
import com.yalingunayer.bastra.commons.domain.Suit
import com.yalingunayer.bastra.server.GameRoomActor.PlayerInformation
import com.yalingunayer.bastra.commons.domain.PlayerState
import com.yalingunayer.bastra.commons.domain.PlayerScore
import com.yalingunayer.bastra.commons.domain.Player

class GameRoomActorSpec extends FlatSpec with Matchers {
  it should "fish when necessary" in {
    val baseDeck = CardStack.sorted
    
    val p1 = Player("foo", "Foo")
    val p2 = Player("bar", "Bar")
    
    val card: Card = "J♠"
    val hand1: List[Card] = List("A♠", "2♠", "3♠", card)
    val hand2: List[Card] = List("A♦", "2♦", "3♦", "4♦")
    val middleStack: List[Card] = List("A♣")
    val deck = baseDeck.removed(hand1 ++ hand2 ++ middleStack)
    
    val player1 = PlayerInformation(PlayerSession(p1, null), PlayerState(hand1, CardStack.empty, PlayerScore.zero))
    val player2 = PlayerInformation(PlayerSession(p2, null), PlayerState(hand2, CardStack.empty, PlayerScore.zero))
    
    val result = GameRoomActor.determineNextState(card, player1, player2, deck.cards, middleStack)
    
    result.winner should be (None)
    result.newState.deck should be (deck)
    result.newState.playerInTurn.state.hand should be (CardStack(hand2))
    result.newState.playerWaiting.state.hand should be (CardStack(hand1).removed(card))
    result.newState.playerWaiting.state.bucket should be (CardStack(card :: middleStack))
    result.newState.middleStack.isEmpty should be (true)
    result.isFished should be (true)
  }
  
  it should "not fish when not possible" in {
    val baseDeck = CardStack.sorted
    
    val p1 = Player("foo", "Foo")
    val p2 = Player("bar", "Bar")
    
    val card: Card = "4♠"
    val hand1: List[Card] = List("A♠", "2♠", "3♠", card)
    val hand2: List[Card] = List("A♦", "2♦", "3♦", "4♦")
    val middleStack: List[Card] = List("A♣")
    val deck = baseDeck.removed(hand1 ++ hand2 ++ middleStack)
    
    val player1 = PlayerInformation(PlayerSession(p1, null), PlayerState(hand1, CardStack.empty, PlayerScore.zero))
    val player2 = PlayerInformation(PlayerSession(p2, null), PlayerState(hand2, CardStack.empty, PlayerScore.zero))
    
    val result = GameRoomActor.determineNextState(card, player1, player2, deck.cards, middleStack)
    
    result.winner should be (None)
    result.newState.deck should be (deck)
    result.newState.playerInTurn.state.hand should be (CardStack(hand2))
    result.newState.playerWaiting.state.hand should be (CardStack(hand1).removed(card))
    result.newState.playerWaiting.state.bucket should be (CardStack.empty)
    result.newState.middleStack should be (CardStack(card :: middleStack))
    result.isFished should be (false)
  }
  
  it should "should end the round and elect a winner when no cards remain" in {
    val baseDeck = CardStack.sorted
    
    val p1 = Player("foo", "Foo")
    val p2 = Player("bar", "Bar")
    
    val hand1 = baseDeck.cards.take(4)
    val hand2 = baseDeck.cards.drop(4).take(4)
    val middleStack = baseDeck.cards.drop(8).take(4)
    val deck = baseDeck.cards.drop(12)
    
    val player1 = PlayerInformation(PlayerSession(p1, null), PlayerState(hand1, CardStack.empty, PlayerScore.zero))
    val player2 = PlayerInformation(PlayerSession(p2, null), PlayerState(hand2, CardStack.empty, PlayerScore.zero))
    
    // simulate the game by running playing the first card on all turns for each player
    def playAllRounds(player: PlayerInformation, opponent: PlayerInformation, remaining: CardStack, middle: CardStack): GameRoomActor.StateResult = {
      def doPlayRound(round: Int, player: PlayerInformation, opponent: PlayerInformation, remaining: CardStack, middle: CardStack): GameRoomActor.StateResult = {
        if (round >= 1000) {
          throw new RuntimeException("The game wasn't finished after 1000 rounds");
        }
        val card = player.state.hand.cards.head
        val result = GameRoomActor.determineNextState(card, player, opponent, remaining.cards, middle.cards)
        
        if (result.winner.isDefined) result
        else {
          val nextState = result.newState
          doPlayRound(round + 1, nextState.playerInTurn, nextState.playerWaiting, result.newState.deck, result.newState.middleStack)
        }
      }
      
      doPlayRound(0, player, opponent, remaining, middle)
    }
    
    val result = playAllRounds(player1, player2, deck, middleStack)
    
    result.winner.isDefined should be (true)
  }
}
