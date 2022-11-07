/*
 * Copyright (c) 2022, Nigel Eke
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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

extension (cells: Cells)
  def bounds: Bounds =
    val head :: rest = cells.toList: @unchecked
    val headBounds   = (head.row, head.row, head.column, head.column)

    def updateBounds(bounds: (Int, Int, Int, Int), cell: Cell) =
      (
        Math.min(bounds._1, cell.row),
        Math.max(bounds._2, cell.row),
        Math.min(bounds._3, cell.column),
        Math.max(bounds._4, cell.column)
      )

    val (minRow, maxRow, minColumn, maxColumn) = rest.foldLeft(headBounds)(updateBounds)
    Bounds(minRow to maxRow, minColumn to maxColumn)

  def rotate(n: Int) =
    val realN = if n >= 0 then n % 4 else (4 + n % 4) % 4
    val bounds = cells.bounds
    val source = for
      sourceRow    <- bounds.rows
      sourceColumn <- bounds.columns
    yield Cell(sourceRow, sourceColumn)

    val targets = Array(
      source,
      for (targetColumn <- bounds.rows.reverse; targetRow <- bounds.columns) yield Cell(targetRow, targetColumn),
      for (targetRow <- bounds.rows.reverse; targetColumn <- bounds.columns.reverse) yield Cell(targetRow, targetColumn),
      for (targetColumn <- bounds.rows; targetRow <- bounds.columns.reverse) yield Cell(targetRow, targetColumn)
    )

    val mapping = source.zip(targets(realN)).toMap
    cells.map(mapping)
