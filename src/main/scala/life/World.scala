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

/** A [World] on which the [Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) will be played out.
  * @param liveCells
  *   The population of live [Cell]s in the current [World].
  * @param maybeBounds
  *   The optional [Bounds] placed on the current [World]. If provided, the edges of the [World] are void, and any [Cell]s 'born'
  *   into the edge immediately fall off the edge. If not provided then the [Bounds] are endless.
  * @param maybeViewport
  *   The optional [Bounds] for the view of the [World] when displaying. If provided, the [World] is only displayed within the range
  *   of the viewport's rows and columns. If not provided then the full [World] is displayed.
  */
final case class World(liveCells: Cells, maybeBounds: Option[Bounds], maybeViewport: Option[Bounds])

object World:
  /** Create a [World] with an initial population of [Cell]s. The [World] will have no boundaries, or restricted viewing.
    * @constructor
    * @param cells
    *   The initial population.
    * @return
    *   The created [World].
    */
  def withCells(cells: Cells) = World(liveCells = cells, None, None)

extension (w: World)
  private def removingOffworlders: World =
    def withinRange(r: Range, i: Int)    = r.contains(i)
    def withinBounds(b: Bounds)(c: Cell) = withinRange(b.rows, c.row) && withinRange(b.columns, c.column)
    def cellsInBounds(b: Bounds)         = w.liveCells.filter(withinBounds(b))
    w.maybeBounds.map(bounds => w.copy(liveCells = cellsInBounds(bounds))).getOrElse(w)

  /** Restrict the boundaries of the current [World]. Any existing [Cell]s outside those boundaries will fall off the edge of the
    * [World].
    * @param bounds
    *   The new bounds.
    * @return
    *   The [World] with the updated boundary, and cells removed.
    */
  def withBounds(bounds: Bounds): World = w.copy(maybeBounds = Some(bounds)).removingOffworlders

  /** Remove the [World]'s boundaries.
    * @return
    */
  def unbounded: World = w.copy(maybeBounds = None)

  /** Set the current viewport for displaying.
    * @param viewport
    *   The new viewport.
    * @return
    *   The updated [World] with the new viewport.
    */
  def withViewport(viewport: Bounds): World = w.copy(maybeViewport = Some(viewport))

  /** Clear any pre-defined viewport. The whole [World] will be viewed when displaying.
    * @return
    *   The updated [World] with any existing viewport removed.
    */
  def withoutViewport: World = w.copy(maybeViewport = None)

  /** Add some additional [Cells] to the current [World].
    * @param cells
    *   The [Cells] to be added.
    * @param offset
    *   The offset within the current [World] where the new [Cells] will be added.
    * @return
    *   The [World] with the additional [Cells].
    */
  def withCells(cells: Cells, offset: Position): World =
    val offsetCells = cells.map(_ + offset)
    w.copy(liveCells = w.liveCells ++ offsetCells).removingOffworlders

  private def isLive(cell: Cell): Boolean = w.liveCells.contains(cell)

  private def neighbourCount(cell: Cell): Int = cell.neighbours.count(isLive)

  /** @return
    *   The next generation of the [World], using the rules of life.
    */
  def nextGeneration: World =
    val allNeighbours   = w.liveCells.map(_.neighbours).flatten
    val allCells        = w.liveCells ++ allNeighbours
    val neighbourCounts = allCells.map(c => (c, w.neighbourCount(c)))
    val dyingCells      = neighbourCounts.filter((_, count) => count < 2 || count > 3).map(_._1)
    val bornCells       = neighbourCounts.filter((_, count) => count == 3).map(_._1)
    w.copy(liveCells = w.liveCells -- dyingCells ++ bornCells).removingOffworlders

  /** @return
    *   A pretty string to aid debug & testing primarily.
    */
  def prettified: String =
    val bounds = w.maybeViewport.getOrElse(w.maybeBounds.getOrElse(w.liveCells.bounds))
    def rowString(r: Int): String =
      (for
        c <- bounds.columns
        cellChar = if w.isLive(Cell(r, c)) then "* " else "  "
      yield cellChar).mkString
    (for r <- bounds.rows
    yield rowString(r)).mkString("\n")
