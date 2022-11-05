package life

import scala.collection.BufferedIterator

/** An [Iterator] to provide each generation from an initial [World]. The iterations will stop when the [World] has repeated itself.
  * Note: On an unbounded board, with a glider, the iterations will continue ad infinitum.
  * @param world
  *   The initial [World].
  */
final case class Generations(world: World) extends Iterator[World]:

  type WorldHistory = (World, List[World])

  private val initial: WorldHistory = (world, List.empty[World])

  private def nextWithHistory(current: WorldHistory): WorldHistory =
    val next = current._1.nextGeneration
    (next, next +: current._2)

  private val iterations = Iterator.iterate(initial)(nextWithHistory).buffered

  /** Peeks ahead to the next generation to determine if there is a `next`.
    * @returns
    *   true if the lookahead is a unique [World], false otherwise.
    */
  override def hasNext: Boolean =
    val peek = iterations.head
    peek._2.count(_ == peek._1) <= 1

  /** @returns
    *   The next world. If the caller does not use `hasNext` then this will return the next generation continually, even if there
    *   are no longer any living [Cell]s, or if history has repeated.
    */
  override def next: World = iterations.next._1
