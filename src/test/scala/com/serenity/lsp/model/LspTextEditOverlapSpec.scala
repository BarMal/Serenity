package com.serenity.lsp.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A document's edits must not overlap (LSP 3.17 §3.16.1): the result of applying them would depend on their order. */
class LspTextEditOverlapSpec extends AnyFlatSpec with Matchers:

  private def edit(startLine: Int, startChar: Int, endLine: Int, endChar: Int): LspTextEdit =
    LspTextEdit(LspRange(LspPosition(startLine, startChar), LspPosition(endLine, endChar)), "x")

  "Edits to one document" should "not overlap when their ranges are apart" in {
    LspTextEdit.overlap(List(edit(0, 0, 0, 3), edit(0, 5, 0, 8), edit(1, 0, 1, 2))) shouldBe false
  }

  it should "overlap when one range starts inside another on the same line" in {
    LspTextEdit.overlap(List(edit(0, 0, 0, 5), edit(0, 3, 0, 8))) shouldBe true
  }

  it should "overlap when a range spans a line that another edits" in {
    LspTextEdit.overlap(List(edit(0, 2, 2, 0), edit(1, 1, 1, 4))) shouldBe true
  }

  it should "overlap when the edits are given out of order" in {
    LspTextEdit.overlap(List(edit(0, 3, 0, 8), edit(0, 0, 0, 5))) shouldBe true
  }

  it should "not overlap when one range ends where the next begins" in {
    LspTextEdit.overlap(List(edit(0, 0, 0, 3), edit(0, 3, 0, 6))) shouldBe false
  }

  it should "not overlap when insertions share a position" in {
    LspTextEdit.overlap(List(edit(0, 4, 0, 4), edit(0, 4, 0, 4))) shouldBe false
  }

  it should "not overlap when an insertion sits at the start of a replaced range" in {
    LspTextEdit.overlap(List(edit(0, 2, 0, 2), edit(0, 2, 0, 6))) shouldBe false
  }

  it should "overlap when an insertion sits inside a replaced range" in {
    LspTextEdit.overlap(List(edit(0, 2, 0, 6), edit(0, 4, 0, 4))) shouldBe true
  }

  it should "not overlap with no edits or one" in {
    (LspTextEdit.overlap(Nil), LspTextEdit.overlap(List(edit(0, 0, 0, 1)))) shouldBe (false, false)
  }
