package game.rules

import game.model._

class Logic {
  val game = Game.startWithNames(List("A", "B", "C", "D")) match {
    case Left(err) => ???
    case Right(game) => game
  }
}
