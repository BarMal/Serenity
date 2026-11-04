package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A preview tab replaced by the next preview closes the old document on the language server (#2479). */
class PreviewReplacementLspSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val first  = Path.of("/workspace/First.scala")
  private val second = Path.of("/workspace/Second.scala")

  private def scalaBuffer(path: Path): Buffer =
    val base = Buffer.fromString(BufferId(0), "object X")
    base.copy(document = base.document.copy(filePath = Some(path), language = Some(LanguageId.Scala)))

  "Replacing the preview tab" should "close the replaced file's document" in {
    val previewed = FileResults.loaded(AppState.initial, first, scalaBuffer(first), OpenIntent.Preview)
    val replaced  = FileResults.loaded(previewed, second, scalaBuffer(second), OpenIntent.Preview)

    LspDocumentSync.closedDocuments(previewed, replaced) shouldBe
      List(LspEffect.FileClosed(first.toUri.toString, LanguageId.Scala))
  }

  it should "close nothing when the previous preview was kept" in {
    val previewed = FileResults.loaded(AppState.initial, first, scalaBuffer(first), OpenIntent.Preview)
    val kept      = FileResults.surfaced(previewed, first, OpenIntent.Keep)
    val opened    = FileResults.loaded(kept, second, scalaBuffer(second), OpenIntent.Preview)

    LspDocumentSync.closedDocuments(kept, opened) shouldBe Nil
  }
