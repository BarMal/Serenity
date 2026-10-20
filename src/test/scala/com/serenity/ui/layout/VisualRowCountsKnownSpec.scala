package com.serenity.ui.layout

import java.awt.Font

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Row counts that take the rows of lines already wrapped the same way instead of wrapping them again. */
class VisualRowCountsKnownSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font   = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val buffer = Buffer.fromString(BufferId(1), "one\ntwo\nthree")

  private def counts(known: Int => Option[Int]): VisualRowCounts =
    VisualRowCounts.forBuffer(buffer, 400, font, None, false, WrappedLineCache.Uncached, known = known)

  "A line the caller knows the rows of" should "count as the caller says, without being wrapped" in {
    counts(Map(1 -> 7).get).rowsIn(1) shouldBe 7
  }

  it should "leave the lines it does not know to be wrapped" in {
    val rows = counts(Map(1 -> 7).get)

    rows.rowsIn(0) shouldBe 1
    rows.rowsIn(2) shouldBe 1
    rows.rowsBetween(0, 3) shouldBe 9
  }

  "Counting with nothing known" should "wrap every line, as before" in {
    counts(_ => None).rowsBetween(0, 3) shouldBe 3
  }
