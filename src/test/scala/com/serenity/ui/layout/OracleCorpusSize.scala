package com.serenity.ui.layout

import scala.util.Try

/** The size of the ligature oracle's random-line test: 10,000 lines by default, and whatever `SERENITY_FUZZ_LINES` asks
  * for when it holds a positive whole number (the scheduled workflow asks for 100,000).
  */
object OracleCorpusSize:

  val Variable: String = "SERENITY_FUZZ_LINES"

  private val Default = 10000

  def fuzzLines(environment: Map[String, String]): Int =
    environment.get(Variable).flatMap(asked => Try(asked.trim.toInt).toOption).filter(_ > 0).getOrElse(Default)
