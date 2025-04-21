package game.model

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class GameSpec extends AnyFunSpec with Matchers{
  describe("Game:") {
    it("creates an initial game value when passed a list of player names") {
      val playerNamesList = List(
        "player1",
        "player2",
        "player3",
        "player4"
      )

      val game = Game.startWithNames(playerNamesList).toOption.getOrElse(fail("Expected Right but got Left"))
      game.players.length shouldBe 4
      game.players.map {
        case (player) => playerNamesList.contains(player.name) shouldBe true
      }
      game.isGameOver shouldBe false
      game.activePlayerIndex should (be >=1 and be <=4)
    }

    it("should return Left if there are more than 5 players"){
      val playerNamesList = List(
        "player1",
        "player2",
        "player3",
        "player4",
        "player5",
        "player6"
      )

      val game = Game.startWithNames(playerNamesList)
      game.isLeft shouldBe true
    }

    it("should return Left if there are less than 2 players"){
      val game = Game.startWithNames(List("player1"))
      game.isLeft shouldBe true
    }
  }
}
