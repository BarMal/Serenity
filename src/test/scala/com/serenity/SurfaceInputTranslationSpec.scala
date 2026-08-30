package com.serenity

import com.serenity.keystroke.events.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Every cell is pinned, including the misses, because the surfaces are not uniform and tidying them into uniformity
  * would be a behaviour change. The asymmetries are deliberate: the runner takes `Paste` and the modal does not; the
  * panel and peek overlays take the plain deletes but not the word deletes.
  */
class SurfaceInputTranslationSpec extends AnyFlatSpec with Matchers:

  /** Every raw event any surface translates, plus events that must stay unmapped. */
  private val allInputs: List[Event] =
    List(
      InsertChar('a'),
      InsertChar('3'),
      InsertChar('0'),
      DeleteBackward,
      DeleteForward,
      DeleteWordBackward,
      DeleteWordForward,
      Paste,
      MoveUp,
      MoveDown,
      MoveLeft,
      MoveRight,
      TabKey,
      ReverseTabKey,
      Enter,
      NewLine,
      Escape,
      FindNext,
      MoveWordLeft,
      SelectAll
    )

  private def check[S](translate: Event => Option[S], expected: Map[Event, S]): Unit =
    allInputs.foreach { event =>
      withClue(s"translating $event: ") {
        translate(event) shouldBe expected.get(event)
      }
    }

  "SurfaceInput.intentOf" should "read the shared vocabulary out of raw input" in
    // Several intent cases share a name with the event they come from (DeleteBackward, Paste, ...). These assertions
    // are what prove each pattern binds the event and yields the intent, rather than resting on scoping rules.
    check(
      SurfaceInput.intentOf,
      Map(
        InsertChar('a')    -> FocusIntent.Insert('a'),
        InsertChar('3')    -> FocusIntent.Insert('3'),
        InsertChar('0')    -> FocusIntent.Insert('0'),
        DeleteBackward     -> FocusIntent.DeleteBackward,
        DeleteForward      -> FocusIntent.DeleteForward,
        DeleteWordBackward -> FocusIntent.DeleteWordBackward,
        DeleteWordForward  -> FocusIntent.DeleteWordForward,
        Paste              -> FocusIntent.Paste,
        MoveUp             -> FocusIntent.Navigate(Direction.Up),
        MoveDown           -> FocusIntent.Navigate(Direction.Down),
        MoveLeft           -> FocusIntent.Navigate(Direction.Left),
        MoveRight          -> FocusIntent.Navigate(Direction.Right),
        TabKey             -> FocusIntent.NextGroup,
        ReverseTabKey      -> FocusIntent.PreviousGroup,
        Enter              -> FocusIntent.Submit,
        NewLine            -> FocusIntent.Submit,
        Escape             -> FocusIntent.Dismiss
      )
    )

  it should "not claim events that are not focus input" in {
    SurfaceInput.intentOf(SelectAll) shouldBe None
    SurfaceInput.intentOf(FindNext) shouldBe None
    SurfaceInput.intentOf(SaveFile) shouldBe None
  }

  "CommandRunnerEvent.fromEvent" should "translate exactly the runner's vocabulary" in
    check(
      CommandRunnerEvent.fromEvent,
      Map(
        InsertChar('a')    -> RunnerInsertChar('a'),
        InsertChar('3')    -> RunnerInsertChar('3'),
        InsertChar('0')    -> RunnerInsertChar('0'),
        DeleteBackward     -> RunnerDeleteBackward,
        DeleteForward      -> RunnerDeleteForward,
        DeleteWordBackward -> RunnerDeleteWordBackward,
        DeleteWordForward  -> RunnerDeleteWordForward,
        Paste              -> RunnerPaste,
        MoveUp             -> RunnerNavigate(Direction.Up),
        MoveDown           -> RunnerNavigate(Direction.Down),
        MoveLeft           -> RunnerNavigate(Direction.Left),
        MoveRight          -> RunnerNavigate(Direction.Right),
        // issue #931: category tabs are retired -- Tab/Shift+Tab no longer translate to anything for the runner.
        Enter              -> RunnerSubmit,
        NewLine            -> RunnerSubmit,
        Escape             -> RunnerDismiss
      )
    )

  it should "pass an already-runner event through unchanged" in {
    CommandRunnerEvent.fromEvent(RunnerSubmit) shouldBe Some(RunnerSubmit)
    CommandRunnerEvent.fromEvent(RunnerSelectVisibleItem(2)) shouldBe Some(RunnerSelectVisibleItem(2))
  }

  "ModalInputEvent.fromEvent" should "translate exactly the modal's vocabulary, which excludes paste" in
    check(
      ModalInputEvent.fromEvent,
      Map(
        InsertChar('a')    -> ModalInsertChar('a'),
        InsertChar('3')    -> ModalInsertChar('3'),
        InsertChar('0')    -> ModalInsertChar('0'),
        DeleteBackward     -> ModalDeleteBackward,
        DeleteForward      -> ModalDeleteForward,
        DeleteWordBackward -> ModalDeleteWordBackward,
        DeleteWordForward  -> ModalDeleteWordForward,
        MoveUp             -> ModalNavigate(Direction.Up),
        MoveDown           -> ModalNavigate(Direction.Down),
        MoveLeft           -> ModalNavigate(Direction.Left),
        MoveRight          -> ModalNavigate(Direction.Right),
        TabKey             -> ModalNextField,
        ReverseTabKey      -> ModalPreviousField,
        Enter              -> ModalSubmit,
        NewLine            -> ModalSubmit,
        FindNext           -> ModalFindNext,
        Escape             -> ModalDismiss
      )
    )

  it should "pass an already-modal event through unchanged" in {
    ModalInputEvent.fromEvent(ModalSubmit) shouldBe Some(ModalSubmit)
    ModalInputEvent.fromEvent(ModalClick("focus", None)) shouldBe Some(ModalClick("focus", None))
  }

  "StartupPageEvent.fromEvent" should "translate only vertical navigation, submit, dismiss and a non-zero digit" in
    check(
      StartupPageEvent.fromEvent,
      Map(
        MoveUp          -> StartupPageMoveUp,
        MoveDown        -> StartupPageMoveDown,
        Enter           -> StartupPageSubmit,
        NewLine         -> StartupPageSubmit,
        Escape          -> StartupPageDismiss,
        InsertChar('3') -> StartupPageSelect(2)
      )
    )

  it should "pass an already-startup-page event through unchanged" in {
    StartupPageEvent.fromEvent(StartupPageSubmit) shouldBe Some(StartupPageSubmit)
  }

  "PanelInputEvent.fromEvent" should "navigate, activate, and treat other text input as returning focus" in
    check(
      PanelInputEvent.fromEvent,
      Map(
        MoveUp          -> PanelInputEvent.Navigate(Direction.Up),
        MoveDown        -> PanelInputEvent.Navigate(Direction.Down),
        MoveLeft        -> PanelInputEvent.Navigate(Direction.Left),
        MoveRight       -> PanelInputEvent.Navigate(Direction.Right),
        InsertChar('a') -> PanelInputEvent.ReturnFocus,
        InsertChar('3') -> PanelInputEvent.ReturnFocus,
        InsertChar('0') -> PanelInputEvent.ReturnFocus,
        DeleteBackward  -> PanelInputEvent.ReturnFocus,
        DeleteForward   -> PanelInputEvent.ReturnFocus,
        TabKey          -> PanelInputEvent.ReturnFocus,
        ReverseTabKey   -> PanelInputEvent.ReturnFocus,
        Escape          -> PanelInputEvent.ReturnFocus,
        Enter           -> PanelInputEvent.Activate,
        NewLine         -> PanelInputEvent.Activate
      )
    )

  it should "pass an already-panel event through unchanged" in {
    PanelInputEvent.fromEvent(PanelInputEvent.NoOp) shouldBe Some(PanelInputEvent.NoOp)
  }

  "PeekInputEvent.fromEvent" should "navigate, accept, dismiss, and mark other text input as foreign" in
    check(
      PeekInputEvent.fromEvent,
      Map(
        MoveUp          -> PeekInputEvent.Navigate(Direction.Up),
        MoveDown        -> PeekInputEvent.Navigate(Direction.Down),
        MoveLeft        -> PeekInputEvent.Navigate(Direction.Left),
        MoveRight       -> PeekInputEvent.Navigate(Direction.Right),
        Enter           -> PeekInputEvent.Accept,
        NewLine         -> PeekInputEvent.Accept,
        Escape          -> PeekInputEvent.Dismiss,
        InsertChar('a') -> PeekInputEvent.OtherInput,
        InsertChar('3') -> PeekInputEvent.OtherInput,
        InsertChar('0') -> PeekInputEvent.OtherInput,
        DeleteBackward  -> PeekInputEvent.OtherInput,
        DeleteForward   -> PeekInputEvent.OtherInput,
        TabKey          -> PeekInputEvent.OtherInput,
        ReverseTabKey   -> PeekInputEvent.OtherInput
      )
    )

  it should "pass an already-peek event through unchanged" in {
    PeekInputEvent.fromEvent(PeekInputEvent.Accept) shouldBe Some(PeekInputEvent.Accept)
  }
