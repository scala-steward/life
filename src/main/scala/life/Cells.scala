package life

import scala.io.*
import scala.util.*
import scala.util.Right

/** [Cells] is a [Cell] collection.
  */
type Cells = Set[Cell]

object Cells:
  type Error  = String
  type Result = Either[Error, Cells]

  private def fromTextEx(s: String): Cells =
    val rows = s
      .replace(" ", "")     // Remove spaces
      .split("\n")          // Split lines
      .filterNot(_.isBlank) // Remove blank lines
      .map(_.trim)          // Remove end of lines
      .toList
    val rowLengths = rows.map(_.length)
    require(s.length > 0, "Invalid empty string")
    require(Set.from(rowLengths).sizeIs == 1, s"Inconsistent row lengths $rowLengths")
    val liveCells = for
      r <- 0 until rows.length
      row = rows(r)
      c <- 0 until row.length if row(c) != '.'
    yield Cell(r, c)
    liveCells.toSet

  /** Create a set of Cells from a multi-lined string representation. The first character of the string represents offset 0, 0.
    * Spaces and blank lines are removed, '.' are empty cells, anything else is a live cell.
    *
    * @param s
    *   The string representation.
    * @returns
    *   Either the [Cells] or an [Error].
    */
  def fromText(s: String): Result =
    Try(fromTextEx(s)) match
      case Success(cells) => Right[Error, Cells](cells)
      case Failure(error) => Left[Error, Cells](error.getMessage)

  private def fromFilenameEx(filename: String): Cells =
    val source = Source.fromFile(filename)(Codec.defaultCharsetCodec)
    fromTextEx(source.getLines().mkString("\n"))

  /** Create a set of Cells from the content of a regular text file. The first character in the file is at offset 0, 0. Spaces and
    * blank lines are removed, '.' are empty cells, anything else is a live cell.
    *
    * @param filename
    *   The filename (and path) of the file cotaining the [Cells] description.
    * @returns
    *   Either the [Cells] or an [Error].
    */
  def fromFilename(filename: String): Result =
    Try(fromFilenameEx(filename)) match
      case Success(cells) => Right[Error, Cells](cells)
      case Failure(error) => Left[Error, Cells](error.getMessage)
