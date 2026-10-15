package com.serenity.ui.layout

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.rope.{Balance, Rope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The scrollbar's thumb is sized from the total visual rows of a document. An edit must update that total from the
  * lines it touched alone, and the total must be the one a recount of the whole document gives.
  */
class VisualRowTotalIncrementalSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferKey = "buffer"
  private val wrapWidth = 12

  private def rowsOfText(text: String): Int = 1 + text.length / wrapWidth

  private val lines = (0 until 300).map(n => s"line $n " + "word " * (n % 9))
  private val base  = Rope(lines.mkString("\n"))

  final private class Session:
    val store    = new VisualLineIndexStore[String](4)
    val measured = AtomicInteger(0)

    def total(content: Rope): Int =
      val counts = store.counts(
        bufferKey,
        content,
        line =>
          val _ = measured.incrementAndGet()
          rowsOfText(content.getLine(line).getOrElse(""))
      )
      counts.rowsAboveAndTotal(0, 0, exact = true)._2

    def totalAfterWarming(content: Rope): Int =
      val _ = total(content)
      measured.set(0)
      0

  private def recounted(content: Rope): Int =
    (0 until content.lineCount).map(line => rowsOfText(content.getLine(line).getOrElse(""))).sum

  private def edit(rope: Option[Rope]): Rope = rope.getOrElse(fail("edit refused"))

  private def offsetOf(line: Int, column: Int): Int = base.lineColumnToOffset(line, column)

  "The total after a single-character edit" should "match a full recount, measuring only the edited line" in {
    val session = Session()
    session.totalAfterWarming(base)
    val edited = edit(base.insert(offsetOf(150, 3), "x"))

    session.total(edited) shouldBe recounted(edited)
    session.measured.get() shouldBe 1
  }

  "The total after joining two lines" should "match a full recount, measuring only the joined line" in {
    val session = Session()
    session.totalAfterWarming(base)
    val joined = edit(base.delete(offsetOf(150, lines(150).length), offsetOf(151, 0)))

    joined.lineCount shouldBe base.lineCount - 1
    session.total(joined) shouldBe recounted(joined)
    session.measured.get() shouldBe 1
  }

  "The total after pasting many lines" should "match a full recount, measuring the pasted lines and at most the line they landed on" in {
    val session = Session()
    session.totalAfterWarming(base)
    val pasted = (0 until 40).map(n => s"pasted $n " + "text " * (n % 5)).mkString("\n") + "\n"
    val edited = edit(base.insert(offsetOf(100, 0), pasted))

    edited.lineCount shouldBe base.lineCount + 40
    session.total(edited) shouldBe recounted(edited)
    session.measured.get() should be <= 41
  }

  "A total asked for again over the same content" should "measure nothing" in {
    val session = Session()
    session.totalAfterWarming(base)

    (0 until 5).foreach(_ => session.total(base) shouldBe recounted(base))

    session.measured.get() shouldBe 0
  }

  "A run of single-character edits" should "never recount the document" in {
    val session = Session()
    session.totalAfterWarming(base)

    val last = (0 until 25).foldLeft(base)((content, step) =>
      val next = edit(content.insert(content.lineColumnToOffset(150, 2), "y"))
      session.total(next) shouldBe recounted(next)
      next
    )

    last.lineCount shouldBe base.lineCount
    session.measured.get() shouldBe 25
  }
