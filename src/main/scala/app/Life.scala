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
