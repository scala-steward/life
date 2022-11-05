package life

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.*

import scala.io.*

class CellsSpec extends AnyWordSpec with Matchers:

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
  }
