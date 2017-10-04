package com.yalingunayer.bastra.commons.domain

import com.yalingunayer.bastra.commons.Utils

abstract class Suit(val name: String, val shortName: String) extends Serializable
abstract class Rank(val value: Int, val name: String, val shortName: String) extends Serializable

object Suit {
  case class Clubs() extends Suit("Clubs", "♣")
  case class Spades() extends Suit("Spades", "♠")
  case class Diamonds() extends Suit("Diamonds", "♦")
  case class Hearts() extends Suit("Hearts", "♥")

  def all: List[Suit] = List(Clubs(), Spades(), Diamonds(), Hearts())
  
  implicit def string2suit(s: String) = s match {
    case "♣" => Clubs()
    case "♠" => Spades()
    case "♦" => Diamonds()
    case "♥" => Hearts()
    case _ => throw new RuntimeException(f"Unknown suit ${s}")
  }
}

object Rank {
  case class Ace() extends Rank(14, "Ace", "A")
  case class Two() extends Rank(2, "Two", "2")
  case class Three() extends Rank(3, "Three", "3")
  case class Four() extends Rank(4, "Four", "4")
  case class One() extends Rank(5, "Five", "5")
  case class Six() extends Rank(6, "Six", "6")
  case class Seven() extends Rank(7, "Seven", "7")
  case class Eight() extends Rank(8, "Eight", "8")
  case class Nine() extends Rank(9, "Nine", "9")
  case class Ten() extends Rank(10, "Ten", "10")
  case class Jack() extends Rank(11, "Jack", "J")
  case class Queen() extends Rank(12, "Queen", "Q")
  case class King() extends Rank(13, "King", "K")

  def all: List[Rank] = List(Ace(), Two(), Three(), Four(), One(), Six(), Seven(), Eight(), Nine(), Ten(), Jack(), Queen(), King())
  
  implicit def string2rank(s: String) = s match {
    case "A" => Ace()
    case "2" => Two()
    case "3" => Three()
    case "4" => Four()
    case "5" => One()
    case "6" => Six()
    case "7" => Seven()
    case "8" => Eight()
    case "9" => Nine()
    case "0" => Ten()
    case "J" => Jack()
    case "Q" => Queen()
    case "K" => King()
    case _ => throw new RuntimeException(f"Unknown rank ${s}")
  }
}

object Card {
  private val pattern = "^([AJKQ2-9])\\s*([♣♠♦♥])$".r
  
  def fullDeck: List[Card] = for {
    suit <- Suit.all
    rank <- Rank.all
  } yield Card(rank, suit)
  
  implicit def string2card(s: String): Card = s match {
    case pattern(rank, suit) => Card(rank, suit)
    case _ => throw new RuntimeException(f"Invalid card string ${s}")
  }
}

object CardStack {
  def sorted: CardStack = CardStack(Card.fullDeck)
  
  def shuffled: CardStack = CardStack(scala.util.Random.shuffle(Card.fullDeck))

  val empty: CardStack = CardStack(List())
  
  implicit def cards2stack(cards: List[Card]): CardStack = CardStack(cards)
}

case class Card(val rank: Rank, val suit: Suit) {
  def name(): String = f"${rank.name} of ${suit.name}"
  def shortName(): String = f"${rank.shortName}${suit.shortName}"
  
  def score(): Int = {
    (rank, suit) match {
      case (Rank.Two(), Suit.Clubs()) => 2
      case (Rank.Ten(), Suit.Diamonds()) => 3
      case (Rank.Jack(), _) => 1
      case (Rank.Ace(), _) => 1
      case _ => 0
    }
  }
  
  def canFish(other: Card) = {
    if (rank == Rank.Jack()) true
    else this == other
  }
  
  override def toString(): String = shortName
}

case class CardStack(val cards: List[Card]) {
  def removed(card: Card): CardStack = CardStack(Utils.removeLast(cards, card))
  def removed(cards: Seq[Card]): CardStack = cards.foldLeft(this)((stack, card) => stack.removed(card))
  def added(card: Card): CardStack = CardStack(cards :+ card)
  
  def isEmpty = cards.isEmpty
  
  override def toString(): String = cards.mkString(", ")
}
