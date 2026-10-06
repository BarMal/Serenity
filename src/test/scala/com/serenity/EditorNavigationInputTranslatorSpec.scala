package com.serenity

import com.serenity.config.*
import com.serenity.input.FocusedInputTranslator
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorNavigationInputTranslatorSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(0)

  private val editorState =
    val initial = AppState.initial
    initial.copy(
      persisted = initial.persisted.copy(
        config = AppConfig.default.withHotkeyConfig(HotkeyConfig.forOs("Linux")),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )

  "FocusedInputTranslator" should "treat Ctrl+Backspace and Ctrl+Delete as word deletion in editor focus" in {
    val translator = FocusedInputTranslator.forState(editorState)

    translator.translate(KeyStrokeInfo(InputKey.Backspace, None, Set(Modifier.Ctrl))) shouldBe DeleteWordBackward
    translator.translate(KeyStrokeInfo(InputKey.Delete, None, Set(Modifier.Ctrl))) shouldBe DeleteWordForward
  }

  it should "treat PageUp, PageDown, Ctrl+Home, and Ctrl+End as file navigation in editor focus" in {
    val translator = FocusedInputTranslator.forState(editorState)

    translator.translate(KeyStrokeInfo(InputKey.PageUp, None, Set.empty)) shouldBe PageUp
    translator.translate(KeyStrokeInfo(InputKey.PageDown, None, Set.empty)) shouldBe PageDown
    translator.translate(KeyStrokeInfo(InputKey.Home, None, Set(Modifier.Ctrl))) shouldBe MoveToStartOfFile
    translator.translate(KeyStrokeInfo(InputKey.End, None, Set(Modifier.Ctrl))) shouldBe MoveToEndOfFile
  }

  it should "treat Shift-arrow keys as selection extension in editor focus" in {
    val translator = FocusedInputTranslator.forState(editorState)
    val shift      = Set(Modifier.Shift)

    translator.translate(KeyStrokeInfo(InputKey.ArrowLeft, None, shift)) shouldBe ExtendSelectionLeft
    translator.translate(KeyStrokeInfo(InputKey.ArrowRight, None, shift)) shouldBe ExtendSelectionRight
    translator.translate(KeyStrokeInfo(InputKey.ArrowUp, None, shift)) shouldBe ExtendSelectionUp
    translator.translate(KeyStrokeInfo(InputKey.ArrowDown, None, shift)) shouldBe ExtendSelectionDown
  }

  it should "treat Ctrl-arrow keys as word navigation in editor focus" in {
    val translator = FocusedInputTranslator.forState(editorState)
    val ctrl       = Set(Modifier.Ctrl)

    translator.translate(KeyStrokeInfo(InputKey.ArrowLeft, None, ctrl)) shouldBe MoveWordLeft
    translator.translate(KeyStrokeInfo(InputKey.ArrowRight, None, ctrl)) shouldBe MoveWordRight
  }

  it should "treat Ctrl+Shift-arrow keys as word selection extension in editor focus" in {
    val translator = FocusedInputTranslator.forState(editorState)
    val ctrlShift  = Set(Modifier.Ctrl, Modifier.Shift)

    translator.translate(KeyStrokeInfo(InputKey.ArrowLeft, None, ctrlShift)) shouldBe ExtendSelectionWordLeft
    translator.translate(KeyStrokeInfo(InputKey.ArrowRight, None, ctrlShift)) shouldBe ExtendSelectionWordRight
  }

  it should "treat Ctrl+Alt-arrow keys as identifier-part navigation in editor focus" in {
    val translator = FocusedInputTranslator.forState(editorState)
    val ctrlAlt    = Set(Modifier.Ctrl, Modifier.Alt)

    translator.translate(KeyStrokeInfo(InputKey.ArrowLeft, None, ctrlAlt)) shouldBe MoveSubWordLeft
    translator.translate(KeyStrokeInfo(InputKey.ArrowRight, None, ctrlAlt)) shouldBe MoveSubWordRight
  }

  it should "treat Ctrl+Alt+Shift-arrow keys as identifier-part selection extension in editor focus" in {
    val translator   = FocusedInputTranslator.forState(editorState)
    val ctrlAltShift = Set(Modifier.Ctrl, Modifier.Alt, Modifier.Shift)

    translator.translate(KeyStrokeInfo(InputKey.ArrowLeft, None, ctrlAltShift)) shouldBe ExtendSelectionSubWordLeft
    translator.translate(KeyStrokeInfo(InputKey.ArrowRight, None, ctrlAltShift)) shouldBe ExtendSelectionSubWordRight
  }
