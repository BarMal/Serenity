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
    FileWorkflowState(
      mode = FileWorkflowMode.SaveAs,
      filename = "notes.odt",
      bufferHasRichFormatting = true
    ).wouldLoseFormatting shouldBe false
    FileWorkflowState(
      mode = FileWorkflowMode.SaveAs,
      filename = "notes.docx",
      bufferHasRichFormatting = true
    ).wouldLoseFormatting shouldBe false
  }

  "SaveAsFileWorkflowState.cycleFormat" should "step forward through SaveFormat.ordered, rewriting the extension" in {
    val workflow = SaveAsFileWorkflowState(filename = "notes.txt")

    val next: SaveAsFileWorkflowState = workflow.cycleFormat(1)
    val nextFilename: String          = next.filename
    nextFilename.shouldBe("notes.md")

    val nextNext: SaveAsFileWorkflowState = next.cycleFormat(1)
    val nextNextFilename: String          = nextNext.filename
    nextNextFilename.shouldBe("notes.rtf")
  }

  it should "step backward through SaveFormat.ordered, wrapping around at the start" in {
    val workflow = SaveAsFileWorkflowState(filename = "notes.txt")

    val previous: SaveAsFileWorkflowState = workflow.cycleFormat(-1)
    val previousFilename: String          = previous.filename
    previousFilename.shouldBe("notes.docx")
  }

  it should "wrap forward from the last format back to the first" in {
    val workflow = SaveAsFileWorkflowState(filename = "notes.docx")

    val filename: String = workflow.cycleFormat(1).filename
    filename.shouldBe("notes.txt")
  }

  it should "append the canonical extension to a filename with none" in {
    val workflow = SaveAsFileWorkflowState(filename = "document")

    val filename: String = workflow.cycleFormat(1).filename
    filename.shouldBe("document.md")
  }

  it should "preserve the base filename exactly when rewriting the extension" in {
    val workflow = SaveAsFileWorkflowState(filename = "my.notes.v2.txt")

    val filename: String = workflow.cycleFormat(1).filename
    filename.shouldBe("my.notes.v2.md")
  }

  it should "treat an unrecognized extension as Text before cycling" in {
    val workflow = SaveAsFileWorkflowState(filename = "notes.scala")

    // Text -> Markdown is the first step forward from the Text fallback.
    val filename: String = workflow.cycleFormat(1).filename
    filename.shouldBe("notes.md")
  }

  it should "clear any status message, like other field-mutating methods" in {
    val workflow = SaveAsFileWorkflowState(filename = "notes.txt", statusMessage = Some("stale"))

    val statusMessage: Option[String] = workflow.cycleFormat(1).statusMessage
    statusMessage.shouldBe(None)
  }

  "FileWorkflowState.cyclableFields" should "cycle Save As focus through filename, format, and path with tab" in {
    val saveAs = SaveAsFileWorkflowState()

    val afterOne: FileWorkflowField   = saveAs.switchField(1).activeField
    val afterTwo: FileWorkflowField   = saveAs.switchField(1).switchField(1).activeField
    val afterThree: FileWorkflowField = saveAs.switchField(1).switchField(1).switchField(1).activeField
    afterOne.shouldBe(FileWorkflowField.Format)
    afterTwo.shouldBe(FileWorkflowField.Path)
    afterThree.shouldBe(FileWorkflowField.Filename)
  }

  it should "leave Open's focus cycle unchanged: filename and path only, no format" in {
    val open = OpenFileWorkflowState()

    val afterOne: FileWorkflowField = open.switchField(1).activeField
    val afterTwo: FileWorkflowField = open.switchField(1).switchField(1).activeField
    afterOne.shouldBe(FileWorkflowField.Path)
    afterTwo.shouldBe(FileWorkflowField.Filename)
  }
