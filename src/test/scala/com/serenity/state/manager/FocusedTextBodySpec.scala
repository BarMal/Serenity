package com.serenity.state.manager

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.{AppState, BufferId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FocusedTextBodySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private def bufferWithContent(text: String, language: Option[LanguageId] = None) =
    val base = AppState.initial.persisted.buffers(bufferId)
    base.copy(document = base.document.copy(content = Rope(text), language = language))

  "FocusedTextBody.activeRange" should "report None when there is no active line" in {
    val buffer = bufferWithContent("alpha\nbeta\ngamma")
    FocusedTextBody.activeRange(buffer, None) shouldBe None
  }

  it should "report None when the active line falls outside the buffer" in {
    val buffer = bufferWithContent("alpha\nbeta\ngamma")
    FocusedTextBody.activeRange(buffer, Some(99)) shouldBe None
  }

  it should "report the contiguous non-blank block around the active line for plain text" in {
    val buffer = bufferWithContent("first\nsecond\n\nfourth\nfifth")
    FocusedTextBody.activeRange(buffer, Some(1)) shouldBe Some(0 to 1)
    FocusedTextBody.activeRange(buffer, Some(3)) shouldBe Some(3 to 4)
  }

  it should "report the whole buffer as one block when there are no blank separators" in {
    val buffer = bufferWithContent("first\nsecond\nthird")
    FocusedTextBody.activeRange(buffer, Some(1)) shouldBe Some(0 to 2)
  }

  it should "report the markdown block around the active line for a markdown buffer" in {
    val buffer =
      bufferWithContent("# Title\n\nfirst paragraph\nstill first\n\nsecond paragraph", Some(LanguageId.Markdown))
    FocusedTextBody.activeRange(buffer, Some(2)) shouldBe Some(2 to 3)
    FocusedTextBody.activeRange(buffer, Some(5)) shouldBe Some(5 to 5)
  }

  "FocusedTextBody.markdownBlock" should "match activeRange's markdown result for the same line" in {
    val buffer = bufferWithContent("# Title\n\nfirst paragraph\nstill first", Some(LanguageId.Markdown))
    FocusedTextBody.markdownBlock(buffer, 2) shouldBe FocusedTextBody.activeRange(buffer, Some(2)).get
  }
