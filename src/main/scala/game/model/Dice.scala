package game.model

import scala.util.Random

sealed trait DieColor extends Product with Serializable

object DieColor {
  case object White1 extends DieColor

  case object White2 extends DieColor

  case object Red extends DieColor

  case object Yellow extends DieColor

  case object Green extends DieColor

  case object Blue extends DieColor
}

case class Die(color: DieColor, value: Int, disabled: Boolean = false)

object Die {
  def apply(color: DieColor): Die = Die(color, 1)
}

case class Dice(dice: Map[DieColor, Die]) {
  def rollAll(): Dice = {
    val rolledDice = dice.map {
      case (color, die) =>
        val updatedDie = if (die.disabled) die else die.copy(value = Random.between(1, 7))
        color -> updatedDie
    }
    copy(dice = rolledDice)
  }

  def disableDie(color: DieColor): Dice = {
    if (color == DieColor.White1 || color == DieColor.White2) this
    else dice.get(color) match {
      case Some(die) => copy(dice = dice.updated(color, die.copy(disabled = true)))
      case None => this
    }
  }
}

object Dice {
  def apply(): Dice = {
    val initialDice = List(
      Die(DieColor.White1),
      Die(DieColor.White2),
      Die(DieColor.Red),
      Die(DieColor.Yellow),
      Die(DieColor.Green),
      Die(DieColor.Blue),
    )
    Dice(initialDice.map(d => d.color -> d).toMap)
  }
}