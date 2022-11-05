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

  /** @return
    *   The [World]'s boundaries. If defined, then that definition will be return. If the [World] is unbounded then the limits of
    *   the existing cells are returned.
    */
  def bounds =
    lazy val calculatedBounds =
      val head :: rest = w.liveCells.toList: @unchecked
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
    w.maybeBounds.getOrElse(calculatedBounds)

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
    * @param row
    *   The row offset in the current [World] where the new [Cells] will be added.
    * @param column
    *   The column offset in the current [World] where the new [Cells] will be added.
    * @return
    *   The [World] with the additional [Cells].
    */
  def withCells(cells: Cells, row: Int, column: Int): World =
    val offsetCells = cells.map(c => Cell(c.row + row, c.column + column))
    w.copy(liveCells = w.liveCells ++ offsetCells).removingOffworlders

  private def isLive(c: Cell): Boolean = w.liveCells.contains(c)

  private def neighboursCount(c: Cell): Int = c.neighbouringCells.count(isLive)

  /** @return
    *   The next generation of the [World], using the rules of life.
    */
  def nextGeneration: World =
    val allNeighbours   = w.liveCells.map(_.neighbouringCells).flatten
    val allCells        = w.liveCells ++ allNeighbours
    val neighbourCounts = allCells.map(c => (c, w.neighboursCount(c)))
    val dyingCells      = neighbourCounts.filter((_, count) => count < 2 || count > 3).map(_._1)
    val bornCells       = neighbourCounts.filter((_, count) => count == 3).map(_._1)
    w.copy(liveCells = w.liveCells -- dyingCells ++ bornCells).removingOffworlders

  /** @return
    *   A pretty string to aid debug & testing primarily.
    */
  def prettified: String =
    val bounds = w.maybeViewport.getOrElse(w.bounds)
    def rowString(r: Int): String =
      (for
        c <- bounds.columns
        cellChar = if w.isLive(Cell(r, c)) then "* " else "  "
      yield cellChar).mkString
    (for r <- bounds.rows
    yield rowString(r)).mkString("\n")
