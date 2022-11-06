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
import scala.util.Random

/** A collection of pre-defined [Cell] patterns to help build [World]s with well-known groupings.
  */
object Patterns:

  private def fromPattern(name: String): Cells =
    val source = Source.fromResource(s"patterns/$name.life")(Codec.defaultCharsetCodec)
    Cells.fromText(source.mkString).toOption.fold(???)(identity)

  /** @return
    *   A two period [Beacon](https://upload.wikimedia.org/wikipedia/commons/1/1c/Game_of_life_beacon.gif)
    */
  def beacon = fromPattern("beacon")

  /** @return
    *   A static
    *   [Beehive](https://upload.wikimedia.org/wikipedia/commons/thumb/6/67/Game_of_life_beehive.svg/196px-Game_of_life_beehive.svg.png)
    */
  def beehive = fromPattern("beehive")

  /** @return
    *   A two period [Blinker](https://upload.wikimedia.org/wikipedia/commons/9/95/Game_of_life_blinker.gif)
    */
  def blinker = fromPattern("blinker")

  /** @return
    *   A static
    *   [Block](https://upload.wikimedia.org/wikipedia/commons/thumb/9/96/Game_of_life_block_with_border.svg/132px-Game_of_life_block_with_border.svg.png)
    */
  def block = fromPattern("block")

  /** @return
    *   A static
    *   [Boat](https://upload.wikimedia.org/wikipedia/commons/thumb/7/7f/Game_of_life_boat.svg/164px-Game_of_life_boat.svg.png)
    */
  def boat = fromPattern("boat")

  /** @return
    *   An iterating [Glider](https://upload.wikimedia.org/wikipedia/commons/f/f2/Game_of_life_animated_glider.gif)
    */
  def glider = fromPattern("glider")

  /** @return
    *   An iterating [Gosper glider
    *   gun](https://upload.wikimedia.org/wikipedia/commons/thumb/e/e0/Game_of_life_glider_gun.svg/610px-Game_of_life_glider_gun.svg.png)
    */
  def gliderGunGosper = fromPattern("gliderGunGosper")

  /** @return
    *   A [Loaf](https://upload.wikimedia.org/wikipedia/commons/thumb/f/f4/Game_of_life_loaf.svg/196px-Game_of_life_loaf.svg.png)
    */
  def loaf = fromPattern("loaf")

  /** @return
    *   A 15 period [Penta-decathlon](https://upload.wikimedia.org/wikipedia/commons/f/fb/I-Column.gif)
    */
  def pentaDecathlon = fromPattern("pentaDecathlon")

  /** @return
    *   A three period [Pulsar](https://upload.wikimedia.org/wikipedia/commons/0/07/Game_of_life_pulsar.gif)
    */
  def pulsar = fromPattern("pulsar")

  /** @return
    *   An iterating [Lightweight Spaceship](https://upload.wikimedia.org/wikipedia/commons/3/37/Game_of_life_animated_LWSS.gif)
    */
  def spaceshipLightweight = fromPattern("spaceshipLightweight")

  /** @return
    *   A two period [Toad](https://upload.wikimedia.org/wikipedia/commons/1/12/Game_of_life_toad.gif)
    */
  def toad = fromPattern("toad")

  /** @return
    *   A static
    *   [Tub](https://upload.wikimedia.org/wikipedia/commons/thumb/3/31/Game_of_life_flower.svg/164px-Game_of_life_flower.svg.png)
    */
  def tub = fromPattern("tub")

  /** Randomize a board's population. Cells are created in approxiamately 20% of the given board.
    * @param board
    *   The [Bounds] of the board to be populated.
    * @return
    *   The randomized population.
    */
  def random(board: Bounds): Cells =
    (for
      row    <- board.rows
      column <- board.columns
      if Random.nextDouble <= 0.2
    yield Cell(row, column)).toSet
