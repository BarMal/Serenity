package com.serenity

import com.serenity.io.FileType
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Pure coverage for the file workflow's format detection and lossy-save warning (issue #1253) -- kept separate from
  * `FileWorkflowStateManagerSpec` (StateManager-level integration) and `ModalSurfaceCompositionSpec` (rendering), since
  * these are plain derived properties of `FileWorkflowState` with no I/O or paint involved.
  */
class FileWorkflowFormatSpec extends AnyFlatSpec with Matchers:

  "FileWorkflowState.detectedFileType" should "detect the format from the typed filename's extension" in {
    FileWorkflowState(mode = FileWorkflowMode.SaveAs, filename = "notes.md").detectedFileType shouldBe FileType.Markdown
    FileWorkflowState(
      mode = FileWorkflowMode.SaveAs,
      filename = "notes.rtf"
    ).detectedFileType shouldBe FileType.RichText
    FileWorkflowState(mode = FileWorkflowMode.SaveAs, filename = "notes.scala").detectedFileType shouldBe FileType.Scala
  }

  it should "default to plain text for a filename with no extension, rather than Unknown" in {
    FileWorkflowState(mode = FileWorkflowMode.SaveAs, filename = "").detectedFileType shouldBe FileType.Text
    FileWorkflowState(mode = FileWorkflowMode.SaveAs, filename = "notes").detectedFileType shouldBe FileType.Text
    FileWorkflowState(mode = FileWorkflowMode.SaveAs, filename = ".gitignore").detectedFileType shouldBe FileType.Text
    FileWorkflowState(mode = FileWorkflowMode.SaveAs, filename = "notes.").detectedFileType shouldBe FileType.Text
  }

  "FileWorkflowState.wouldLoseFormatting" should "be false when the buffer carries no rich formatting" in {
    FileWorkflowState(
      mode = FileWorkflowMode.SaveAs,
      filename = "notes.txt",
      bufferHasRichFormatting = false
    ).wouldLoseFormatting shouldBe false
  }

  it should "be true when a rich buffer is saved at a format that can't preserve formatting" in {
    FileWorkflowState(
      mode = FileWorkflowMode.SaveAs,
      filename = "notes.txt",
      bufferHasRichFormatting = true
    ).wouldLoseFormatting shouldBe true
    FileWorkflowState(
      mode = FileWorkflowMode.SaveAs,
      filename = "notes.md",
      bufferHasRichFormatting = true
    ).wouldLoseFormatting shouldBe true
  }

  it should "be false when a rich buffer is saved at a format that does preserve formatting" in {
    FileWorkflowState(
      mode = FileWorkflowMode.SaveAs,
      filename = "notes.rtf",
      bufferHasRichFormatting = true
    ).wouldLoseFormatting shouldBe false
  }
