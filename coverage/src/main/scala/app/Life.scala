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

package app

import life.*

import com.typesafe.config.*

import scala.io.*
import scala.util.*

object Life:

  val config: Config = ConfigFactory.load().getConfig("life")
  val board          = Bounds.from(config.getConfig("defaults.boardBounds"))
  val viewport       = Bounds.from(config.getConfig("defaults.viewportBounds"))

  private def iterateGenerations(cells: Cells): Unit =
    val initialWorld = World.withCells(cells).withBounds(board).withViewport(viewport)
    print(Ansi.clearScreen)
    Generations(initialWorld).foreach { world =>
      print(Ansi.home)
      println(world.prettified)
      Thread.sleep(200)
    }

  private def runRandom: Unit =
    val cells = Patterns.random(board)
    iterateGenerations(cells)

  private def runWithFile(filename: String): Unit =
    Cells.fromFilename(filename) match
      case Right(cells) => iterateGenerations(cells)
      case Left(error)  => println(error)

  def main(args: Array[String]): Unit =
    if args.isEmpty
    then runRandom
    else args.foreach(runWithFile)

  object Ansi:
    private val esc: Char = 0x1b
    val clearScreen       = s"$esc[2J"
    val home              = s"$esc[H"
