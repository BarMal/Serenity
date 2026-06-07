package com.serenity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.serenity.keystroke.events.*
import com.serenity.ui.layout.ViewportSize

class EventHierarchySpec extends AnyFlatSpec with Matchers:

  "Event hierarchy" should "classify editor input events with strong types" in {
    InsertChar('a').isInstanceOf[EditorEvent] shouldBe true
    InsertChar('a').isInstanceOf[TextInputEvent] shouldBe true

    DeleteBackward.isInstanceOf[EditorEvent] shouldBe true
    DeleteBackward.isInstanceOf[DeletionEvent] shouldBe true
    DeleteWordBackward.isInstanceOf[EditorEvent] shouldBe true
    DeleteWordBackward.isInstanceOf[DeletionEvent] shouldBe true
    DeleteWordForward.isInstanceOf[EditorEvent] shouldBe true
    DeleteWordForward.isInstanceOf[DeletionEvent] shouldBe true

    MoveLeft.isInstanceOf[EditorEvent] shouldBe true
    MoveLeft.isInstanceOf[NavigationEvent] shouldBe true
  }

  it should "classify hotkeys as application events while preserving text-entry compatibility" in {
    Save.isInstanceOf[AppEvent] shouldBe true
    Save.isInstanceOf[HotkeyEvent] shouldBe true
    Save.isInstanceOf[TextEntryEvent] shouldBe true

    ToggleCommandRunner.isInstanceOf[AppEvent] shouldBe true
    NextTab.isInstanceOf[AppEvent] shouldBe true
    PreviousTab.isInstanceOf[AppEvent] shouldBe true
  }

  it should "classify system-originated events separately from editor actions" in {
    ResizeEvent(ViewportSize(120, 40)).isInstanceOf[SystemEvent] shouldBe true
  }
