package life

/** Bounds for a board or viewport.
  * @param rows
  *   Range for the row boundary.
  * @param columns
  *   Range for the column boundary.
  */
final case class Bounds(rows: Range, columns: Range)

object Bounds:
  import com.typesafe.config.*

  /** Extract [Bounds] from a [Config]. The [Config] must be the root for the [Bounds] definition. The definition requires each of
    * `minRow`, `maxRow`, `minColumn` and `maxColumn` to be defined, e.g.,
    *
    * ```
    * mybounds {
    *   minRow: 0
    *   maxRow: 40
    *   minColumn: 0
    *   maxColumn: 80
    * }
    * ```
    *
    * @param config
    *   The root [Config] object
    * @return
    *   The [Bounds] object derived from the [Config] settings.
    */
  def from(config: Config) =
    def intFor(s: String) = config.getInt(s)
    Bounds(
      intFor("minRow") to intFor("maxRow"),
      intFor("minColumn") to intFor("maxColumn")
    )
