package com.serenity.ui.tui

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2365: a grapheme cluster that takes two cells (an emoji sequence) is one wide cell: its first codepoint is the
  * glyph and the rest of the cluster rides along as marks. After any cluster of more than one codepoint the ANSI moves
  * the cursor to an absolute position, so a terminal that counts the cluster by its codepoints (a legacy one) keeps the
  * rest of the row where the buffer has it.
  */
class TerminalEmojiClusterSpec extends AnyFlatSpec with Matchers:

  private def s(codePoints: Int*): String = String(codePoints.toArray, 0, codePoints.length)

  private val esc = 0x1b.toChar.toString

  private val clusters = Vector(
    "variation selector 16" -> s(0x2764, 0xfe0f),
    "emoji modifier"        -> s(0x1f44d, 0x1f3fd),
    "joined family"         -> s(0x1f468, 0x200d, 0x1f469, 0x200d, 0x1f467),
    "flag"                  -> s(0x1f1fa, 0x1f1f8),
    "keycap"                -> s('1'.toInt, 0xfe0f, 0x20e3),
    "rainbow flag"          -> s(0x1f3f3, 0xfe0f, 0x200d, 0x1f308),
    "tag flag"              -> s(0x1f3f4, 0xe0067, 0xe0062, 0xe0065, 0xe006e, 0xe0067, 0xe007f)
  )

  private def buffer(width: Int = 12, height: Int = 2): TerminalScreenBuffer = new TerminalScreenBuffer(width, height)

  "putString" should "make an emoji cluster one wide cell carrying the rest of the cluster, and put the next character after it" in
    clusters.foreach { (name, cluster) =>
      val buf = buffer()
      buf.putString(0, 0, "a" + cluster + "b")
      val frame = buf.snapshot
      withClue(name) {
        frame(0, 0).text shouldBe "a"
        frame(1, 0).text shouldBe cluster
        frame(1, 0).codePoint shouldBe cluster.codePointAt(0)
        frame(1, 0).span shouldBe CellSpan.Wide
        frame(2, 0).span shouldBe CellSpan.Continuation
        frame(3, 0).text shouldBe "b"
      }
    }

  it should "leave a variation selector after a base that has no emoji presentation in its one cell" in {
    val buf = buffer()
    buf.putString(0, 0, s('a'.toInt, 0xfe0f) + "b")
    buf.snapshot(0, 0).span shouldBe CellSpan.Narrow
    buf.snapshot(1, 0).text shouldBe "b"
  }

  it should "drop a cluster's rest when something else is written over its cell" in {
    val buf = buffer()
    buf.putString(0, 0, clusters(2)._2)
    buf.putString(0, 0, "x ")
    buf.snapshot(0, 0).text shouldBe "x"
    buf.snapshot(1, 0).text shouldBe " "
  }

  "flushDiff" should "move the cursor to an absolute position after an emoji cluster" in {
    val buf = buffer()
    buf.putString(0, 0, "a" + clusters(0)._2 + "b")
    buf.flushDiff(scrollRegions = false, startPen = None).ansi should include(esc + "[1;4Hb")
  }

  it should "move the cursor to an absolute position after a decomposed accent as well" in {
    val buf = buffer()
    buf.putString(0, 0, s('e'.toInt, 0x0301) + "x")
    buf.flushDiff(scrollRegions = false, startPen = None).ansi should include(esc + "[1;2Hx")
  }

  it should "not move the cursor between plain ASCII or wide CJK cells" in {
    val ascii = buffer()
    ascii.putString(0, 0, "abcd")
    ascii.flushDiff(scrollRegions = false, startPen = None).ansi should include("abcd")
    val cjk = buffer()
    cjk.putString(0, 0, "中文x")
    cjk.flushDiff(scrollRegions = false, startPen = None).ansi should include("中文x")
  }

  "A frame interpreted by a terminal that counts a cluster by its codepoints" should "still have the rest of the row where the buffer put it" in
    clusters.foreach { (name, cluster) =>
      val buf = buffer()
      buf.putString(0, 0, "a" + cluster + "b")
      val ansi  = buf.flushDiff(scrollRegions = false, startPen = None).ansi
      val shown = TerminalEmulator.blank(12, 2).consume(ansi).frame
      withClue(name) {
        shown(0, 0).text shouldBe "a"
        shown(1, 0).codePoint shouldBe cluster.codePointAt(0)
        shown(3, 0).text shouldBe "b"
      }
    }
