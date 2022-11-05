package life

/** A [Cell] an a board, specifying its position.
  * @param row
  *   The [Cell]'s row.
  * @param column
  *   The [Cell]'s column.
  */
final case class Cell(row: Int, column: Int)

extension (cell: Cell)
  /** @returns
    *   The neighbouring [Cell]s of the current [Cell].
    */
  def neighbouringCells: List[Cell] =
    (for
      deltaRow    <- -1 to 1
      deltaColumn <- -1 to 1
      if !(deltaRow == 0 && deltaColumn == 0)
    yield Cell(cell.row + deltaRow, cell.column + deltaColumn)).toList
