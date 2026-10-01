package com.serenity.state.reducers

import java.nio.file.Paths

import com.serenity.command.RichTextIntent
import com.serenity.config.AppMode
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.{InlineMark, ParagraphAlignment, ParagraphRole}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Formatting commands on a Markdown file edit its source text, and the toolbar reads its formatting back from it. */
class MarkdownFormattingReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(1)
  private val paneId   = PaneId(0)

  private def markdown(text: String, cursor: Cursor): AppState =
    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(editing = EditingState.fromCursors(List(cursor)))
    val document = buffer.document.copy(filePath = Some(Paths.get("notes.md")), language = Some(LanguageId.Markdown))
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer.copy(document = document)),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        )
      ),
      runtime = AppState.initial.runtime.copy(nextBufferId = BufferId(bufferId.value + 1))
    )

  private def selecting(line: Int, from: Int, to: Int): Cursor =
    Cursor(Selection(CursorPosition(line, from), CursorPosition(line, to)))

  private def reduced(intent: RichTextIntent, state: AppState): (Buffer, ReducerResult) =
    val result = RichTextReducer.reduce(intent, state)
    AppStateValidation.validationErrors(result.state) shouldBe Nil
    (result.state.persisted.buffers.getOrElse(bufferId, fail("buffer gone")), result)

  "Bold in a Markdown file" should "wrap the selection in the source, keeping the word selected, as one undo step" in {
    val (after, result) =
      reduced(RichTextIntent.ToggleRichTextMark(InlineMark.Bold), markdown("say hello", selecting(0, 4, 9)))

    after.document.content.collect() shouldBe "say **hello**"
    after.allSelections shouldBe List(Selection(CursorPosition(0, 6), CursorPosition(0, 11)))
    after.document.isDirty shouldBe true
    after.richText.richTextDocument shouldBe None
    result.effects.collect { case AppEffect.Undo(effect) => effect } should have size 1
  }

  it should "unwrap it again on a second toggle" in {
    val bolded = RichTextReducer.reduce(
      RichTextIntent.ToggleRichTextMark(InlineMark.Bold),
      markdown("say hello", selecting(0, 4, 9))
    )

    val (after, _) = reduced(RichTextIntent.ToggleRichTextMark(InlineMark.Bold), bolded.state)

    after.document.content.collect() shouldBe "say hello"
    after.allSelections shouldBe List(Selection(CursorPosition(0, 4), CursorPosition(0, 9)))
  }

  "A heading in a Markdown file" should "set the caret line's # prefix" in {
    val (after, _) = reduced(
      RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Heading(2)),
      markdown("Title\nbody", Cursor(CursorPosition(0, 3)))
    )

    after.document.content.collect() shouldBe "## Title\nbody"
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 6))
  }

  "Routing in a Markdown file" should "apply what Markdown can spell and refuse what it can't" in {
    val state = markdown("say hello", selecting(0, 4, 9))

    RichTextReducer.route(RichTextIntent.ToggleRichTextMark(InlineMark.Underline), state) shouldBe RichTextRoute.Apply
    RichTextReducer.route(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.Body), state) shouldBe
      RichTextRoute.Apply
    RichTextReducer.route(RichTextIntent.SetRichTextFontFamily("Menlo"), state) shouldBe
      RichTextRoute.Refuse("Markdown has no syntax for fonts or text sizes.")
    RichTextReducer.route(RichTextIntent.SetRichTextParagraphAlignment(ParagraphAlignment.Center), state) shouldBe
      RichTextRoute.Refuse("Markdown has no syntax for paragraph alignment.")
    RichTextReducer.route(RichTextIntent.SetRichTextParagraphRole(ParagraphRole.dropCap()), state) shouldBe
      RichTextRoute.Refuse("Markdown has no syntax for drop caps.")
  }

  private def toolbarFor(state: AppState): List[ContextualToolbarItem] =
    ContextualToolbar.itemsFor(
      state.copy(persisted = state.persisted.copy(config = state.persisted.config.withAppMode(AppMode.Prose)))
    )

  private def selectedButtons(items: List[ContextualToolbarItem]): List[String] =
    items.collect { case button: ContextualToolbarItem.Button if button.selected => button.id }

  "The toolbar on a Markdown file" should "light the emphasis the source gives the caret's word" in {
    val items = toolbarFor(markdown("say ***hello***", Cursor(CursorPosition(0, 9))))

    selectedButtons(items).filter(Set("bold", "italic", "underline")) shouldBe List("bold", "italic")
  }

  it should "offer Body and H1-H6 at the caret line's level, without drop cap, fonts, colour or alignment" in {
    val items = toolbarFor(markdown("intro\n### Title", Cursor(CursorPosition(1, 6))))
    val role = items.collectFirst {
      case dropdown: ContextualToolbarItem.Dropdown if dropdown.id == "paragraph-role" =>
        dropdown.optionItem
    }

    role.map(_.options.map(_.label)) shouldBe Some(List("Body", "H1", "H2", "H3", "H4", "H5", "H6"))
    role.map(_.selectedIndex) shouldBe Some(3)
    items.map(_.id).filter(id => id.startsWith("font") || id.startsWith("color") || id.startsWith("align")) shouldBe Nil
  }

  it should "keep only the view controls outside prose mode" in {
    val state = markdown("say hello", Cursor(CursorPosition(0, 0)))
    val codeMode =
      state.copy(persisted = state.persisted.copy(config = state.persisted.config.withAppMode(AppMode.Code)))

    ContextualToolbar.itemsFor(codeMode).map(_.id) shouldBe ContextualToolbar.markdownItems.map(_.id)
  }
