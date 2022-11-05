package life

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.*

class CellSpec extends AnyWordSpec with Matchers:

  "A Cell" should {

    "die" when {
      "fewer than two live neighbours, as if by underpopulation" in {
        for initialCells <- Cells.fromText("""
            |. . .
            |. x .
            |. . .
            |""".stripMargin)
        yield
          val world     = World.withCells(initialCells)
          val nextWorld = world.nextGeneration
          nextWorld.liveCells should be(empty)
      }

      "more than three live neighbours, as if by overpopulation" in {
        for
          initialCells <- Cells.fromText("""
            |x . . . x
            |x x . x x
            |. . x . .
            |x x . x x
            |x . . . x
            |""".stripMargin)
          expectedCells <- Cells.fromText("""
            |x x . x x
            |x x x x x
            |. . . . .
            |x x x x x
            |x x . x x
            |""".stripMargin)
        yield
          val world     = World.withCells(initialCells)
          val nextWorld = world.nextGeneration
          nextWorld.liveCells should contain theSameElementsAs (expectedCells)
      }
    }

    "survive" when {
      "two or three live neighbours" in {
        for
          initialCells <- Cells.fromText("""
            |x x . . . .
            |x x . . . .
            |. . . . x .
            |. . . . x .
            |. . . . x .
            |""".stripMargin)
          expectedCells <- Cells.fromText("""
            |x x . . . .
            |x x . . . .
            |. . . . . .
            |. . . x x x
            |. . . . . .
            |""".stripMargin)
        yield
          val world     = World.withCells(initialCells)
          val nextWorld = world.nextGeneration
          nextWorld.liveCells should contain theSameElementsAs (expectedCells)
      }
    }

    "be born" when {
      "three live neighbours, as if by reproduction" in {
        for
          initialCells <- Cells.fromText("""
            |. x .
            |. x .
            |. x .
            |""".stripMargin)
          expectedCells <- Cells.fromText("""
              |. . .
              |x x x
              |. . .
              |""".stripMargin)
        yield
          val world     = World.withCells(initialCells)
          val nextWorld = world.nextGeneration
          nextWorld.liveCells should contain theSameElementsAs (expectedCells)
      }
    }

  }
