package com.serenity.rope

import com.serenity.perf.SettledAllocation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RopeLinesAllocationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val lineLength = 60
  private val lineCount  = 200
  private val lines      = (0 until lineCount).map(index => (index.toString + " ").padTo(lineLength, 'x'))
  private val rope       = Rope(lines.mkString("\n"))

  private def walk(): Int = rope.linesIteratorFrom(0).foldLeft(0)((count, _) => count + 1)

  "Rope.linesIteratorFrom" should "yield each line exactly" in {
    rope.linesIteratorFrom(0).map(_._2).toVector shouldBe lines.toVector
  }

  it should "copy a line that sits inside one chunk only once" in {
    SettledAllocation.perCall(() => walk(), () => walk()) match
      case Some((bytes, _)) =>
        val perLine = bytes.toDouble / lineCount
        withClue(s"$perLine bytes per line: ")(perLine should be < 320.0)
      case None => info("per-thread allocation counter unsupported -- skipping")
  }
