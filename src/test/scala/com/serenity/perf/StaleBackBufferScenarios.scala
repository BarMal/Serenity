package com.serenity.perf

import scala.util.Random

import com.serenity.keystroke.events.{DeleteBackward, Enter, Event, InsertChar}
import com.serenity.richtext.{RichTextDocument, RichTextParagraph, RichTextRun}

/** The document and the random edits the stale-back-buffer specs share. */
trait StaleBackBufferScenarios extends ColdFrameHarness:

  protected val scales = List(1.0, 2.0)

  protected def words(count: Int, seed: String): String =
    (1 to count).map(n => s"${seed}word$n").mkString(" ")

  protected def body(wordCount: Int, seed: String): RichTextParagraph =
    RichTextParagraph(List(RichTextRun(words(wordCount, seed))))

  protected def prose: RichTextDocument =
    RichTextDocument((1 to 8).toList.map(n => body(40 + 7 * n, s"p$n")))

  /** Typing leans towards a delete straight after, so runs often go A, B, A. */
  protected def randomEdit(random: Random): (Event, String) =
    if random.nextInt(12) == 0 then Enter -> "Enter"
    else if random.nextInt(5) < 2 then
      val char = "WWiM m".charAt(random.nextInt(6))
      InsertChar(char) -> s"type '$char'"
    else DeleteBackward -> "delete"
