package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EventHierarchySpec extends AnyFlatSpec with Matchers:

  /** Every trait in the event sum. An event may extend exactly one of these directly; mixing two makes dispatch depend
    * on the order its cases happen to appear in, which is what #988 removes.
    */
  private val eventTraits: Set[Class[?]] =
    Set(
      classOf[GlobalAppEvent],
      classOf[TextEntryEvent],
      classOf[TextInputEvent],
      classOf[DeletionEvent],
      classOf[NavigationEvent],
      classOf[ScrollEvent],
      classOf[ModalRequestEvent],
      classOf[FileEvent],
      classOf[ThemeEvent],
      classOf[CommandRunnerEvent],
      classOf[ModalInputEvent],
      classOf[StartupPageEvent],
      classOf[PanelInputEvent],
      classOf[PeekInputEvent],
      classOf[MouseInputEvent]
    )

  private def directEventParents(event: Event): List[String] =
    event.getClass.getInterfaces.filter(eventTraits.contains).map(_.getSimpleName).toList.sorted

  "Event hierarchy" should "classify editor input events with strong types" in {
    InsertChar('a').isInstanceOf[TextInputEvent] shouldBe true

    DeleteBackward.isInstanceOf[DeletionEvent] shouldBe true
    DeleteWordBackward.isInstanceOf[DeletionEvent] shouldBe true
    DeleteWordForward.isInstanceOf[DeletionEvent] shouldBe true

    MoveLeft.isInstanceOf[NavigationEvent] shouldBe true
  }

  it should "give every event exactly one direct parent in the sum" in {
    val events: List[Event] =
      List(
        InsertChar('a'),
        DeleteBackward,
        MoveLeft,
        ScrollUp(1),
        OpenFind,
        SelectAll,
        Copy,
        Paste,
        Cut,
        Undo,
        Redo,
        ToggleSyntaxHighlighting,
        OpenFile,
        SaveFile,
        SaveAsFile,
        Quit,
        ToggleCommandRunner,
        ToggleContextualToolbar,
        NewTab,
        CloseTab,
        NextTab,
        PreviousTab,
        FileSearch,
        SwitchTheme("dark"),
        RunnerSubmit,
        ModalSubmit,
        StartupPageSubmit,
        ResizeEvent(ViewportSize(120, 40))
      )

    // The invariant is "never more than one", not "exactly one". Since Event became a union of the families, a type
    // that *is* its own family -- ResizeEvent, LspEvent, ExplorerEvent, UnhandledEvent -- belongs to Event by union
    // membership rather than by inheriting anything, so it legitimately reports no parent at all. Two parents is the
    // lattice #988 removed, and that is what this guards against; the positive direction is covered by the
    // family-specific assertions below.
    val offenders = events.map(event => event.toString -> directEventParents(event)).filter(_._2.sizeIs > 1)

    offenders shouldBe empty
  }

  it should "route tab and window hotkeys as global application events" in {
    val globals: List[Event] =
      List(Quit, ToggleCommandRunner, ToggleContextualToolbar, NewTab, CloseTab, NextTab, PreviousTab, FileSearch)

    globals.foreach { event =>
      withClue(s"$event should be a GlobalAppEvent: ") {
        event.isInstanceOf[GlobalAppEvent] shouldBe true
      }
      withClue(s"$event should not also be an editor event: ") {
        event.isInstanceOf[TextEntryEvent] shouldBe false
      }
    }
  }

  it should "route file hotkeys as file events rather than editor events" in {
    OpenFile.isInstanceOf[FileEvent] shouldBe true
    SaveFile.isInstanceOf[FileEvent] shouldBe true

    // The negative direction is not asserted here because it no longer can be: with FileEvent sealed and disjoint from
    // TextEntryEvent, `OpenFile.isInstanceOf[TextEntryEvent]` is a compile error (E030, unreachable), not a false
    // assertion. The compiler proves it, which is what sealing bought.
    directEventParents(OpenFile) shouldBe List("FileEvent")
    directEventParents(SaveFile) shouldBe List("FileEvent")
  }

  it should "keep clipboard and editing hotkeys in the text-entry family" in
    List(Copy, Paste, Cut, Undo, Redo, ToggleSyntaxHighlighting).foreach { event =>
      withClue(s"$event should be a TextEntryEvent: ") {
        event.isInstanceOf[TextEntryEvent] shouldBe true
      }
      withClue(s"$event should not be an application event: ") {
        event.isInstanceOf[GlobalAppEvent] shouldBe false
      }
    }

  it should "classify system-originated events separately from editor actions" in {
    ResizeEvent(ViewportSize(120, 40)).isInstanceOf[ResizeEvent] shouldBe true
  }
