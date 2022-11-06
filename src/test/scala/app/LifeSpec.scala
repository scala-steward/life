package app

import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.*

import scala.io.*

class LifeSpec extends AnyWordSpec with Matchers:

  "The Life" should {

    "use provided text file content for the initial world" in {
      val resource = getClass.getResource("/initialWorld.life")
      val filename = resource.toURI.getPath
      Life.main(Array(filename))
    }

    "exit with error" when {
      "the file does not exist" in {
        Life.main(Array("noWorldFile.life"))
      }

      "the file content is invalid" in {
        val resource = getClass.getResource("/invalidWorld.life")
        val filename = resource.toURI.getPath
        Life.main(Array(filename))
      }
    }

    "use random content" when {
      "no filename provided" in { Life.main(Array.empty) }
    }

  }
