package com.serenity.state.reducers

import scala.util.Random

import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.testkit.UniqueTextEdits
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Carrying annotations across an undo or reload by walking the two ropes together (#1930) must land them exactly where
  * trimming the common prefix and suffix of the two texts does.
  */
class AnnotationRemapAcrossReplacementSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def oracle(annotations: Annotations, before: Rope, after: Rope): Annotations =
    val beforeText = before.collect()
    val afterText  = after.collect()
    val limit      = math.min(beforeText.length, afterText.length)
    val prefix     = Iterator.range(0, limit).takeWhile(i => beforeText(i) == afterText(i)).size
    val suffix = Iterator
      .range(0, limit - prefix)
      .takeWhile(i => beforeText(beforeText.length - 1 - i) == afterText(afterText.length - 1 - i))
      .size
    if prefix == beforeText.length && prefix == afterText.length then annotations
    else
      val edit = EditorEditSupport.MultiCursorEdit(
        0,
        prefix,
        beforeText.length - suffix,
        afterText.substring(prefix, afterText.length - suffix)
      )
      EditorEditSupport.adjustAnnotations(annotations, before, after, List(edit))

  private def randomEdit(random: Random, text: String): String =
    val start = random.nextInt(text.length + 1)
    val end   = math.min(text.length, start + random.nextInt(6))
    text.substring(0, start) + random.alphanumeric.take(random.nextInt(5)).mkString + text.substring(end)

  "Remapping annotations across a replacement" should "agree with trimming the texts' common prefix and suffix" in {
    val random = new Random(1930)
    (1 to 300).foreach { _ =>
      val lines  = Vector.fill(1 + random.nextInt(40))(random.alphanumeric.take(random.nextInt(60)).mkString)
      val text   = lines.mkString("\n")
      val edited = (0 to random.nextInt(3)).foldLeft(text)((current, _) => randomEdit(random, current))
      val annotations = Annotations(
        bookmarks = List(CursorPosition(random.nextInt(lines.size), random.nextInt(4))),
        placeholders = List(Placeholder(CursorPosition(random.nextInt(lines.size), random.nextInt(4)), "n"))
      )
      val before = Rope(text)
      val after  = Rope(edited)

      EditorEditSupport.adjustAnnotationsAcrossReplacement(annotations, before, after) shouldBe
        oracle(annotations, before, after)
    }
  }

  it should "land annotations where the recorded change says, as it does from the texts" in {
    val random = new Random(1838)
    UniqueTextEdits.cases(seed = 1838, count = 300).foreach { found =>
      val annotations = Annotations(
        bookmarks = List(CursorPosition(random.nextInt(found.lineCount), random.nextInt(4))),
        placeholders = List(Placeholder(CursorPosition(random.nextInt(found.lineCount), random.nextInt(4)), "n"))
      )
      val before = Document(found.before)
      val edited = before.edited(found.after, found.change)

      EditorEditSupport.adjustAnnotationsAcross(annotations, before, edited) shouldBe
        EditorEditSupport.adjustAnnotationsAcrossReplacement(annotations, found.before, found.after)
    }
  }
