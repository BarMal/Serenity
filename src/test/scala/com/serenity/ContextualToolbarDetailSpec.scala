package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.richtext.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Keyboard-driven focus and detail lifecycle: typing stays live with the toolbar open, Enter executes the focused
  * control or opens its dropdown/input detail, edits inside an open detail apply on Enter, and Escape closes a detail
  * before dismissing the toolbar. Placement/positioning is covered in [[ContextualToolbarPlacementSpec]], mouse
  * interaction in [[ContextualToolbarMouseSpec]], and display-mode/rendering behaviour in
  * [[ContextualToolbarDisplaySpec]].
  */
class ContextualToolbarDetailSpec extends AnyFlatSpec with Matchers with ContextualToolbarTestSupport:

  "Contextual toolbar" should "leave editor typing active while the toolbar is open" in {
    val stateManager = createStateManager("ContextualToolbarSpec-editor-focus")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("alpha")),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 5)))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    stateManager.applyEvent(InsertChar('!')).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    state.persisted.buffers(bufferId).document.content.toString shouldBe "alpha!"
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "keep the formatted run state when the caret sits on its trailing boundary" in {
    val stateManager = createStateManager("ContextualToolbarSpec-caret-boundary-style")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val document = RichTextDocument
          .fromPlainText("alpha beta gamma")
          .applyMark(
            RichTextRange(RichTextPosition(0, 6), RichTextPosition(0, 10)),
            InlineMark.Bold
          )
          .setFontFamily(
            RichTextRange(RichTextPosition(0, 6), RichTextPosition(0, 10)),
            "Serif"
          )
          .setFontSize(
            RichTextRange(RichTextPosition(0, 6), RichTextPosition(0, 10)),
            18.0f
          )
          .setColor(
            RichTextRange(RichTextPosition(0, 6), RichTextPosition(0, 10)),
            "#336699"
          )
          .normalized
        val nextBuffer = state.persisted
          .buffers(bufferId)
          .copy(
            document =
              state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("alpha beta gamma")),
            editing =
              state.persisted.buffers(bufferId).editing.copy(selection = None, cursors = List(CursorPosition(0, 10))),
            richText = state.persisted.buffers(bufferId).richText.copy(richTextDocument = Some(document))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, nextBuffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    toolbarButton(state, "bold").selected shouldBe true
    toolbarInput(state, "font-family-text").inputItem.currentValue shouldBe "Serif"
    toolbarInput(state, "font-size").inputItem.currentValue shouldBe "18"
    toolbarInput(state, "color-hex").inputItem.currentValue shouldBe "#336699"
  }

  it should "execute the focused formatting command on Enter" in {
    val stateManager = createStateManager("ContextualToolbarSpec-enter")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 6), CursorPosition(0, 10))
        val nextBuffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("alpha beta")),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(selection = Some(selection), cursors = List(selection.focus))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, nextBuffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    focusToolbar(stateManager)
    stateManager.applyEvent(Enter).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    val buffer   = state.persisted.buffers(bufferId)
    buffer.richText.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .map(_.style.marks)
      .shouldBe(Some(Set(InlineMark.Bold)))
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "dismiss on Escape and restore editor focus" in {
    val stateManager = createStateManager("ContextualToolbarSpec-escape")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    focusToolbar(stateManager)
    stateManager.applyEvent(Escape).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.contextualToolbarSurface shouldBe None
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "open a focused font size field with the current value prefilled, accept edits, and apply them on Enter" in {
    val stateManager = createStateManager("ContextualToolbarSpec-font-size")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    moveToolbarFocusTo(stateManager, "font-size")
    stateManager.applyEvent(Enter).unsafeRunSync()

    toolbarStateFrom(stateManager.getCurrentState.unsafeRunSync()).detailState shouldBe
      Some(ContextualToolbarDetailState.Input("font-size", "18"))

    stateManager.applyEvent(DeleteBackward).unsafeRunSync()
    stateManager.applyEvent(DeleteBackward).unsafeRunSync()
    stateManager.applyEvent(InsertChar('2')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('0')).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    val buffer   = state.persisted.buffers(bufferId)
    buffer.richText.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .flatMap(_.style.fontSize)
      .shouldBe(Some(20.0f))
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "retain its intrinsic compact width when a font-family detail opens" in {
    val stateManager = createStateManager("ContextualToolbarSpec-font-family-compact-width")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val beforeWidth = toolbarRect(stateManager.getCurrentState.unsafeRunSync()).width
    moveToolbarFocusTo(stateManager, "font-family")
    stateManager.applyEvent(Enter).unsafeRunSync()

    val opened = stateManager.getCurrentState.unsafeRunSync()
    toolbarStateFrom(opened).detailState.getOrElse(fail("Expected font-family dropdown")) shouldBe
      a[ContextualToolbarDetailState.Dropdown]
    toolbarRect(opened).width shouldBe beforeWidth
  }

  it should "open a focused font family field with the current value prefilled, accept edits, and apply them on Enter" in {
    val stateManager = createStateManager("ContextualToolbarSpec-font-family-input")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    moveToolbarFocusTo(stateManager, "font-family-text")
    stateManager.applyEvent(Enter).unsafeRunSync()

    toolbarStateFrom(stateManager.getCurrentState.unsafeRunSync()).detailState shouldBe
      Some(ContextualToolbarDetailState.Input("font-family-text", "A"))

    stateManager.applyEvent(DeleteBackward).unsafeRunSync()
    "Serif".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    val buffer   = state.persisted.buffers(bufferId)
    buffer.richText.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .flatMap(_.style.fontFamily)
      .shouldBe(Some("Serif"))
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "open a focused color field with the current value prefilled, accept hex edits, and apply them on Enter" in {
    val stateManager = createStateManager("ContextualToolbarSpec-color-input")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    moveToolbarFocusTo(stateManager, "color-hex")
    stateManager.applyEvent(Enter).unsafeRunSync()

    toolbarStateFrom(stateManager.getCurrentState.unsafeRunSync()).detailState shouldBe
      Some(ContextualToolbarDetailState.Input("color-hex", "#336699"))

    (0 until 7).foreach(_ => stateManager.applyEvent(DeleteBackward).unsafeRunSync())
    "ff6600".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val bufferId = activeBufferId(state)
    val buffer   = state.persisted.buffers(bufferId)
    buffer.richText.richTextDocument
      .flatMap(_.paragraphs.headOption)
      .flatMap(_.runs.find(_.text == "beta"))
      .flatMap(_.style.color)
      .shouldBe(Some("#ff6600"))
    state.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "close an open toolbar control on Escape before dismissing the toolbar" in {
    val stateManager = createStateManager("ContextualToolbarSpec-escape-detail")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()
    focusToolbar(stateManager)
    moveToolbarFocusTo(stateManager, "paragraph-role")

    stateManager.applyEvent(Enter).unsafeRunSync()
    stateManager.applyEvent(Escape).unsafeRunSync()

    val afterFirstEscape = stateManager.getCurrentState.unsafeRunSync()
    afterFirstEscape.contextualToolbarSurface should not be empty
    afterFirstEscape.persisted.focus shouldBe Focus.Surface(
      afterFirstEscape.contextualToolbarSurface.getOrElse(fail("Expected toolbar surface")).id
    )

    stateManager.applyEvent(Escape).unsafeRunSync()

    val afterSecondEscape = stateManager.getCurrentState.unsafeRunSync()
    afterSecondEscape.contextualToolbarSurface shouldBe None
    afterSecondEscape.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }
