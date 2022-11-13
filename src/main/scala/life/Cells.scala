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
    val headBounds   = Bounds(head.row to head.row, head.column to head.column)

    def updateBounds(bounds: Bounds, cell: Cell) =
      bounds match
        case Bounds(rows, columns) =>
          val updatedRows    = Math.min(rows.min, cell.row) to Math.max(rows.max, cell.row)
          val updatedColumns = Math.min(columns.min, cell.column) to Math.max(columns.max, cell.column)
          Bounds(updatedRows, updatedColumns)

    rest.foldLeft(headBounds)(updateBounds)

  def rotate(n: Int) =
    val order   = 4
    val modulus = n % order
    val cycle   = if modulus >= 0 then modulus else order + modulus

    val bounds               = cells.bounds
    val boundsRow            = IndexedSeq.from(bounds.rows)
    val boundsRowReversed    = IndexedSeq.from(boundsRow.reverse)
    val boundsColumn         = IndexedSeq.from(bounds.columns)
    val boundsColumnReversed = IndexedSeq.from(boundsColumn.reverse)

    def rotate0(cell: Cell)   = cell // indentity
    def rotate90(cell: Cell)  = Cell(cell.column, boundsRowReversed(cell.row))
    def rotate180(cell: Cell) = Cell(boundsRowReversed(cell.row), boundsColumnReversed(cell.column))
    def rotate270(cell: Cell) = Cell(boundsColumnReversed(cell.column), cell.row)

    val rotate = IndexedSeq(rotate0, rotate90, rotate180, rotate270)(cycle)
    cells.map(rotate)
