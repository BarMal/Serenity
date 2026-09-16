package com.serenity

import com.serenity.config.AppMode
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.RichTextDocument
import com.serenity.rope.Rope
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditingContextSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def bufferWith(language: Option[LanguageId], richText: Boolean = false): Buffer =
    Buffer(
      id = BufferId(0),
      document = Document(Rope("hello"), language = language),
      richText = RichTextState(richTextDocument = Option.when(richText)(RichTextDocument.fromPlainText("hello")))
    )

  private def stateWith(buffer: Buffer, mode: AppMode = AppMode.Code, tui: Boolean = false): AppState =
    val base = AppState.initial
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        config = base.persisted.config.withAppMode(mode)
      ),
      runtime = base.runtime.copy(isTuiMode = tui)
    )

  "EditingContext" should "classify the active buffer by what it holds" in {
    EditingContext.bufferKind(bufferWith(None)) shouldBe BufferKind.PlainText
    EditingContext.bufferKind(bufferWith(None, richText = true)) shouldBe BufferKind.RichText
    EditingContext.bufferKind(bufferWith(Some(LanguageId.Markdown))) shouldBe BufferKind.Markdown
    EditingContext.bufferKind(bufferWith(Some(LanguageId.Scala))) shouldBe BufferKind.Code(LanguageId.Scala)
  }

  it should "derive workspace mode, buffer kind, shell and focus from the state" in {
    val context = stateWith(bufferWith(Some(LanguageId.Scala)), AppMode.Prose, tui = true).editingContext

    context.mode shouldBe AppMode.Prose
    context.buffer shouldBe Some(BufferKind.Code(LanguageId.Scala))
    context.shell shouldBe Shell.Tui
    context.focus shouldBe AppState.initial.persisted.focus
    context.isProseWorkspace shouldBe true
    context.bufferIsCode shouldBe true
  }

  it should "offer code tooling only in a code workspace, whatever the buffer holds" in {
    stateWith(bufferWith(Some(LanguageId.Scala)), AppMode.Code).editingContext.hasCodeTooling shouldBe true
    stateWith(bufferWith(Some(LanguageId.Scala)), AppMode.Prose).editingContext.hasCodeTooling shouldBe false
    stateWith(bufferWith(None), AppMode.Code).editingContext.hasCodeTooling shouldBe true
  }

  it should "report no buffer when nothing is open" in {
    val empty = AppState.empty(com.serenity.config.AppConfig.default)
    empty.editingContext.buffer shouldBe None
    empty.editingContext.shell shouldBe Shell.Gui
  }
