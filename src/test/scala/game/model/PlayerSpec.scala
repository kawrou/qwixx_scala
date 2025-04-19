package game.model

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class PlayerSpec extends AnyFunSpec  with Matchers with TableDrivenPropertyChecks {
  describe("Player") {
    it("should return a player with a name, id, GameCard, and penalty") {
      val player = Player(1, "Test player", GameCard.empty, 0)
      player.id shouldBe 1
      player.name shouldBe "Test player"
      player.penalty shouldBe 0
      player.gameCard.rows.keySet should contain allOf(RowColor.Red, RowColor.Yellow, RowColor.Green, RowColor.Blue)
      player.gameCard.rows.values.forall(_.isEmpty) shouldBe true
    }

    it("should return an updated player when marking a number in the game card") {
      val player = Player(1, "Test player", GameCard.empty, 0)
      val updatedPlayer = Player.markNumber(player, RowColor.Red, 3)
      updatedPlayer.name shouldBe "Test player"
      updatedPlayer.id shouldBe 1
      updatedPlayer.penalty shouldBe 0
      updatedPlayer.gameCard.rows(RowColor.Red) should contain only 3
    }
  }
}
