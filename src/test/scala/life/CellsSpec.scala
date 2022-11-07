package life

import org.scalacheck.Gen
import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.io.*

class CellsSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers:

  "Cells" should {
    "be creatable" when {
      "empty cells" in {
        for cells <- Cells.fromText(".")
        yield cells should be(Set.empty)
      }

      "full cells" in {
        for cells <- Cells.fromText("x")
        yield cells should contain theSameElementsAs (Set(Cell(0, 0)))
      }

      "many rows" in {
        for cells <- Cells.fromText("""
          |. . .
          |. x .
          |. . .
          |""".stripMargin)
        yield cells should contain theSameElementsAs (Set(Cell(1, 1)))
      }

      "empty rows" in {
        for cells <- Cells.fromText("""
          |. . .
          |
          |. x .
          |
          |. . .
          |
          |""".stripMargin)
        yield cells should contain theSameElementsAs (Set(Cell(1, 1)))
      }

      "from a file" in {
        val resource = getClass.getResource("/initialWorld.life")
        val filename = resource.toURI.getPath
        for cells <- Cells.fromFilename(filename)
        yield cells should be(Set(Cell(0, 0), Cell(0, 1), Cell(1, 0), Cell(1, 1)))
      }
    }

    "not be creatable" when {
      "empty" in {
        Cells.fromText("") match
          case Left(text)   => text should be("requirement failed: Invalid empty string")
          case Right(cells) => fail(s"Unexpected cells ${cells.toString}")
      }

      "rows different lengths" in {
        Cells.fromText("""
            |. . . . .
            |. . .
            |. . . . .""".stripMargin) match
          case Left(text)   => text should be("requirement failed: Inconsistent row lengths List(5, 3, 5)")
          case Right(cells) => fail(s"Unexpected cells ${cells.toString}")
      }

      "missing file" in {
        Cells.fromFilename("noWorldFile.life") match
          case Left(text)   => text should include("noWorldFile.life")
          case Right(cells) => fail(s"Unexpected cells ${cells.toString}")
      }

      "invalid file" in {
        val resource = getClass.getResource("/invalidWorld.life")
        val filename = resource.toURI.getPath
        Cells.fromFilename(filename) match
          case Left(text)   => text should be("requirement failed: Inconsistent row lengths List(3, 2, 1)")
          case Right(cells) => fail(s"Unexpected cells ${cells.toString}")
      }
    }

    "rotate" when {
      val cellsOrError = Cells.fromText("""
          |* * * *
          |. . * *
          |. . . *
          |""".stripMargin)

      "0 degrees" in {
        for
          cells <- cellsOrError
          expectedCells <- Cells.fromText("""
              |* * * *
              |. . * *
              |. . . *
              |""".stripMargin)
        yield cells.rotate(0) should be(expectedCells)
      }

      "90 degrees" in {
        for
          cells <- cellsOrError
          expectedCells <- Cells.fromText("""
              |. . *
              |. . *
              |. * *
              |* * *
              |""".stripMargin)
        yield cells.rotate(1) should be(expectedCells)
      }

      "180 degrees" in {
        for
          cells <- cellsOrError
          expectedCells <- Cells.fromText("""
              |* . . .
              |* * . .
              |* * * *
              |""".stripMargin)
        yield cells.rotate(2) should be(expectedCells)
      }

      "270 degrees" in {
        for
          cells <- cellsOrError
          expectedCells <- Cells.fromText("""
              |* * *
              |* * .
              |* . .
              |* . .
              |""".stripMargin)
        yield cells.rotate(3) should be(expectedCells)
      }

      "arbitary number of 90 degree rotations" in {
        forAll(Gen.chooseNum(-100, 100)) { n =>
          for cells <- cellsOrError
          yield cells.rotate(n) should be(a[Cells])
        }
      }
    }
  }
