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
  }
}
