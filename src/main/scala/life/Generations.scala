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
