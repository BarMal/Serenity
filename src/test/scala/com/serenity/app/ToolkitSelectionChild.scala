package com.serenity.app

import cats.effect.unsafe.implicits.global

/** Run by [[ToolkitSelectionLazinessSpec]] in a separate JVM whose class loading the spec reads. Reads the environment
  * and makes the choice exactly as `Main` does before any AWT class loads, then reports it on stdout.
  */
object ToolkitSelectionChild:

  def main(args: Array[String]): Unit =
    val decision = ToolkitSelection.currentEnvironment.map(ToolkitSelection.choose).unsafeRunSync()
    System.out.println(s"chose ${decision.choice} because ${decision.reason}")
    System.exit(0)
