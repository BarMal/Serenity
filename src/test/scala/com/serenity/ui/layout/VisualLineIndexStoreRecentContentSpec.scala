package com.serenity.ui.layout

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.rope.{Balance, Rope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A key's index is kept for the few contents it described last, so going back to one -- an undo, a redo, a frame
  * alternating between two states -- finds its measurements standing instead of following an edit back to them.
  */
class VisualLineIndexStoreRecentContentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferKey = "buffer"

  private def text(edit: String): Rope = Rope(
    (0 until 40).map(line => if line == 20 then edit else s"line $line").mkString("\n")
  )

  final private class Counting:
    val measured = AtomicInteger(0)

    def rowsOf(line: Int): Int =
      val _ = measured.incrementAndGet()
      1 + line % 3

  private def totalRows(store: VisualLineIndexStore[String], content: Rope, counting: Counting): Int =
    store.counts(bufferKey, content, counting.rowsOf).rowsBetween(0, content.lineCount)

  "Going back to content the store described a moment ago" should "find its measurements without measuring again" in {
    val store    = new VisualLineIndexStore[String](4)
    val counting = Counting()
    val before   = text("before")
    val after    = text("after the edit")
    val first    = totalRows(store, before, counting)
    val _        = totalRows(store, after, counting)
    counting.measured.set(0)

    totalRows(store, before, counting) shouldBe first
    counting.measured.get() shouldBe 0
  }

  it should "do so again and again as the content alternates" in {
    val store    = new VisualLineIndexStore[String](4)
    val counting = Counting()
    val before   = text("before")
    val after    = text("after the edit")
    val _        = totalRows(store, before, counting)
    val _        = totalRows(store, after, counting)
    counting.measured.set(0)

    (0 until 10).foreach { _ =>
      totalRows(store, before, counting)
      totalRows(store, after, counting)
    }

    counting.measured.get() shouldBe 0
  }

  it should "still measure the edited line when the content is one it has not seen" in {
    val store    = new VisualLineIndexStore[String](4)
    val counting = Counting()
    val _        = totalRows(store, text("before"), counting)
    counting.measured.set(0)

    totalRows(store, text("a different edit"), counting)

    counting.measured.get() shouldBe 1
  }

  "Content too old to be remembered" should "be followed to as before" in {
    val store    = new VisualLineIndexStore[String](4)
    val counting = Counting()
    val oldest   = text("oldest")
    val _        = totalRows(store, oldest, counting)
    (1 to VisualLineIndexStore.RecentContents + 1).foreach(n => totalRows(store, text(s"edit $n"), counting))
    counting.measured.set(0)

    totalRows(store, oldest, counting) should be > 0
    counting.measured.get() shouldBe 1
  }
