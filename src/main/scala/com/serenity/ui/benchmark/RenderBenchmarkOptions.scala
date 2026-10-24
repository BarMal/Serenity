package com.serenity.ui.benchmark

import cats.syntax.all.*

/** `--render-benchmark`: how big a window, how many frames of each scenario are timed, and how many go unmeasured
  * first.
  */
final case class RenderBenchmarkOptions(widthPx: Int = 1600, heightPx: Int = 1000, frames: Int = 200, warmup: Int = 30)

object RenderBenchmarkOptions:

  private val Size = raw"(\d+)x(\d+)".r

  def parseSize(text: String): Either[String, (Int, Int)] =
    val parsed = text.trim match
      case Size(width, height) => (width.toIntOption, height.toIntOption).tupled.filter((w, h) => w > 0 && h > 0)
      case _                   => None
    parsed.toRight(s"Unknown size '$text'; expected WIDTHxHEIGHT in pixels, for example 1600x1000.")
