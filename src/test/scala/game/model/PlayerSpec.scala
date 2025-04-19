package game.model

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class PlayerSpec extends AnyFunSpec  with Matchers with TableDrivenPropertyChecks {
  describe("Player") {
    it("should return a player with a name, id, GameCard, and penalty") {
      val player = Player.newPlayer(1, "Test player").
        toOption.
        getOrElse(fail("Expected Right but got Left(InvalidName)"))

      player.id shouldBe 1
      player.name shouldBe "Test player"
      player.penalty shouldBe 0
      player.gameCard.rows.keySet should contain allOf(RowColor.Red, RowColor.Yellow, RowColor.Green, RowColor.Blue)
      player.gameCard.rows.values.forall(_.isEmpty) shouldBe true
    }

    it("should return an InvalidName error when player name is an empty string") {
      val result = Player.newPlayer(1, "")
      result shouldBe Left(InvalidName)
      result match {
        case Left(err) => err.message shouldBe "Player name must be a non-empty string."
        case Right(_) => fail(s"Expected Left but got Right")
      }
    }
  }
}
