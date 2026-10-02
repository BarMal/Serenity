package com.serenity

import com.serenity.command.{Command, CommandIntent, SessionIntent}
import com.serenity.input.FocusedInputTranslator
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.rope.Balance
import com.serenity.session.SessionId
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, ModalStateReducer}
import com.serenity.ui.layout.ListPickerComposition
import com.serenity.ui.widget.{Loadable, TextField}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A [[ListPicker]]: arrows move, Enter runs the highlighted choice's command, and a choice that waits on slow work
  * keeps the picker open, showing it, until that work lands or Escape abandons it.
  */
class ModalListPickerReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def command(name: String): Command =
    Command.typed(name, name, CommandIntent.Session(SessionIntent.OpenNamedSession(SessionId(name))))

  private val quick = ListChoice("Quick", None, command("quick"))
  private val slow  = ListChoice("Slow", Some("takes a while"), command("slow"), waitingLabel = Some("Opening Slow…"))

  private def stateWith(picker: ListPicker): AppState =
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("picker"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("picker"),
            SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  private def shown(state: AppState): Option[ListPicker] =
    state.modalSurface
      .map(_.content)
      .collect { case SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)) => picker }

  private val ready = ListPicker.of("Pick one", List(quick, slow), emptyMessage = "Nothing to pick")

  "A list picker" should "close and run a choice that doesn't wait" in {
    val result = ModalEventReducer.reduce(ModalType.ListPicker, Enter, stateWith(ready))

    result.state.modalSurface shouldBe None
    result.effects shouldBe List(AppEffect.ExecuteCommand(quick.action))
  }

  it should "stay open on a choice that waits, showing it as pending, and ignore Enter until it lands" in {
    val onSlow = ModalEventReducer.reduce(ModalType.ListPicker, MoveDown, stateWith(ready)).state
    val picked = ModalEventReducer.reduce(ModalType.ListPicker, Enter, onSlow)

    shown(picked.state).flatMap(_.pending) shouldBe Some(slow)
    picked.effects shouldBe List(AppEffect.ExecuteCommand(slow.action))
    ModalEventReducer.reduce(ModalType.ListPicker, Enter, picked.state).effects shouldBe Nil
  }

  it should "wrap its highlight and take a clicked row" in {
    shown(ModalEventReducer.reduce(ModalType.ListPicker, MoveUp, stateWith(ready)).state)
      .flatMap(_.selectedChoice) shouldBe Some(slow)
    val click = ModalClick(
      ListPickerComposition.choiceActionId(1).value,
      Some(ListPickerComposition.choiceActionId(1).value)
    )
    shown(ModalEventReducer.reduce(ModalType.ListPicker, click, stateWith(ready)).state)
      .flatMap(_.selectedChoice) shouldBe Some(slow)
  }

  it should "pick nothing while its choices are still loading, and close on Escape" in {
    val loading = stateWith(ListPicker.loading("Pick one"))

    ModalEventReducer.reduce(ModalType.ListPicker, Enter, loading).effects shouldBe Nil
    ModalEventReducer.reduce(ModalType.ListPicker, Escape, loading).state.modalSurface shouldBe None
  }

  it should "say so when there is nothing to pick" in {
    ListPicker.of("Pick one", Nil, emptyMessage = "Nothing to pick").items shouldBe Loadable.Empty("Nothing to pick")
  }

  it should "move its highlight with Left and Right when it has no query, and ignore typing" in {
    shown(ModalEventReducer.reduce(ModalType.ListPicker, MoveRight, stateWith(ready)).state)
      .flatMap(_.selectedChoice) shouldBe Some(slow)
    val typing = ModalEventReducer.reduce(ModalType.ListPicker, InsertChar('s'), stateWith(ready))
    typing.state shouldBe stateWith(ready)
    typing.effects shouldBe Nil
  }

  private def previewed(label: String, detail: String): ListChoice =
    val name = label.toLowerCase
    ListChoice(label, Some(detail), command(name), preview = Some(command(s"preview-$name")))

  private val alpha   = previewed("Alpha", "first")
  private val beta    = previewed("Beta", "second")
  private val gamma   = previewed("Gamma", "alphabet soup")
  private val restore = command("restore")

  private val filterable = ListPicker.filterable("Pick", Vector(alpha, beta, gamma), onDismiss = Some(restore))

  private def after(events: Seq[Event], picker: ListPicker = filterable): (AppState, List[AppEffect]) =
    events.foldLeft((stateWith(picker), List.empty[AppEffect])) {
      case ((state, effects), event) =>
        val result = ModalEventReducer.reduce(ModalType.ListPicker, event, state)
        (result.state, effects ++ result.effects)
    }

  private def typed(text: String): Seq[Event] = text.map(InsertChar(_))

  private def query(state: AppState): Option[TextField] = shown(state).flatMap(_.query)

  private def visibleChoices(state: AppState): Option[Vector[ListChoice]] =
    shown(state).flatMap(_.items.toOption).map(_.items)

  private def previewEffect(choice: ListChoice): AppEffect =
    AppEffect.ExecuteCommandUnrecorded(choice.preview.getOrElse(fail(s"${choice.label} has no preview")))

  "A filterable list picker" should "open on every choice with an empty query, previewing nothing" in {
    val opened = ModalStateReducer.show(Modal.ListPicker(filterable), AppState.initial)

    opened.effects shouldBe Nil
    filterable.query shouldBe Some(TextField())
    filterable.items.toOption.map(_.items) shouldBe Some(Vector(alpha, beta, gamma))
    filterable.selectedChoice shouldBe Some(alpha)
  }

  it should "keep, in source order, the choices whose label or detail contains the query in any case" in {
    val (state, _) = after(typed("AL"))

    query(state).map(_.text) shouldBe Some("AL")
    visibleChoices(state) shouldBe Some(Vector(alpha, gamma))
  }

  it should "reset the highlight to the first match on every query change, previewing it" in {
    val (state, effects) = after(MoveDown +: typed("a"))

    visibleChoices(state) shouldBe Some(Vector(alpha, beta, gamma))
    shown(state).flatMap(_.selectedChoice) shouldBe Some(alpha)
    effects shouldBe List(previewEffect(beta), previewEffect(alpha))
  }

  it should "say there are no matches, and preview the first match again once the query matches" in {
    val (empty, emptyEffects) = after(typed("z"))
    shown(empty).map(_.items) shouldBe Some(Loadable.Empty("No matches"))
    emptyEffects shouldBe Nil

    val (restored, restoredEffects) = after(typed("z") :+ DeleteBackward)
    visibleChoices(restored) shouldBe Some(Vector(alpha, beta, gamma))
    restoredEffects shouldBe List(previewEffect(alpha))
  }

  it should "not preview when a query change leaves the same choice highlighted" in {
    after(typed("alp"))._2 shouldBe Nil
  }

  it should "move the query's caret with Left and Right instead of the highlight" in {
    val (state, effects) = after(typed("bta") ++ List(MoveLeft, MoveLeft, InsertChar('e'), MoveRight))

    query(state) shouldBe Some(TextField("beta", 3))
    shown(state).flatMap(_.selectedChoice) shouldBe Some(beta)
    // "b" highlights Beta, "bt" matches nothing, and the inserted "e" brings Beta back.
    effects shouldBe List(previewEffect(beta), previewEffect(beta))
  }

  it should "edit the query with forward, word-backward and word-forward deletes" in {
    val (forward, _) = after(typed("betaz") ++ List(MoveLeft, DeleteForward))
    query(forward).map(_.text) shouldBe Some("beta")

    val (wordBack, _) = after(typed("al soup") :+ DeleteWordBackward)
    query(wordBack).map(_.text) shouldBe Some("al ")

    val (wordForward, _) = after(typed("al soup") ++ List.fill(4)(MoveLeft) :+ DeleteWordForward)
    query(wordForward).map(_.text) shouldBe Some("al ")
  }

  it should "paste the clipboard into the query on one line" in {
    def pasting(clipboard: String) =
      val base = stateWith(filterable)
      ModalEventReducer.reduce(
        ModalType.ListPicker,
        Paste,
        base.copy(runtime = base.runtime.copy(clipboard = Some(clipboard)))
      )

    val pasted = pasting("bet")
    query(pasted.state).map(_.text) shouldBe Some("bet")
    visibleChoices(pasted.state) shouldBe Some(Vector(beta, gamma))
    pasted.effects shouldBe List(previewEffect(beta))

    query(pasting("al\nsoup").state).map(_.text) shouldBe Some("al soup")
  }

  it should "preview the highlighted choice as arrows, Tab and clicks move to it" in {
    after(List(MoveDown))._2 shouldBe List(previewEffect(beta))
    after(List(MoveUp))._2 shouldBe List(previewEffect(gamma))
    after(List(TabKey))._2 shouldBe List(previewEffect(beta))
    val click = ModalClick(
      ListPickerComposition.choiceActionId(2).value,
      Some(ListPickerComposition.choiceActionId(2).value)
    )
    after(List(click))._2 shouldBe List(previewEffect(gamma))
  }

  it should "run its dismiss command, unrecorded, after closing on Escape" in {
    val (state, effects) = after(typed("be") :+ Escape)

    state.modalSurface shouldBe None
    effects.lastOption shouldBe Some(AppEffect.ExecuteCommandUnrecorded(restore))
  }

  it should "run the picked choice's command, recorded, on Enter" in {
    val (state, effects) = after(typed("gam") :+ Enter)

    state.modalSurface shouldBe None
    effects.lastOption shouldBe Some(AppEffect.ExecuteCommand(gamma.action))
  }

  it should "ignore typing and pasting while pending, but still close on Escape" in {
    val waiting = alpha.copy(waitingLabel = Some("Loading Alpha…"))
    val pending =
      ListPicker.filterable("Pick", Vector(waiting, beta), onDismiss = Some(restore)).copy(pending = Some(waiting))

    val (typedState, typedEffects) = after(typed("b") ++ List(Paste, MoveDown), pending)
    typedState shouldBe stateWith(pending)
    typedEffects shouldBe Nil

    val (dismissed, dismissEffects) = after(List(Escape), pending)
    dismissed.modalSurface shouldBe None
    dismissEffects shouldBe List(AppEffect.ExecuteCommandUnrecorded(restore))
  }

  private val many = (0 until 20).map(index => previewed(s"Item$index", s"row $index")).toVector

  private val long = ListPicker.of("Many", many, emptyMessage = "Nothing to pick")

  private def highlightedIn(state: AppState): Option[ListChoice] = shown(state).flatMap(_.selectedChoice)

  "A list picker without a query" should "jump to its first and last rows with Home and End, previewing them" in {
    val (atEnd, endEffects) = after(List(ModalLineEnd), long)
    highlightedIn(atEnd) shouldBe many.lastOption
    endEffects shouldBe many.lastOption.map(previewEffect).toList

    val (atStart, startEffects) = after(List(ModalLineEnd, ModalLineStart), long)
    highlightedIn(atStart) shouldBe Some(many(0))
    startEffects shouldBe List(previewEffect(many(19)), previewEffect(many(0)))
  }

  it should "page by one screen less a row, stopping at either end rather than wrapping" in {
    val pageStep = ListPickerComposition.VisibleRows - 1

    val (paged, pagedEffects) = after(List(ModalPage(1), ModalPage(1)), long)
    highlightedIn(paged) shouldBe Some(many(2 * pageStep))
    pagedEffects shouldBe List(previewEffect(many(pageStep)), previewEffect(many(2 * pageStep)))

    val (bottom, _) = after(List.fill(4)(ModalPage(1)), long)
    highlightedIn(bottom) shouldBe many.lastOption

    val (back, _) = after(List(ModalLast, ModalPage(-1)), long)
    highlightedIn(back) shouldBe Some(many(19 - pageStep))

    val (top, topEffects) = after(List(ModalPage(-1)), long)
    highlightedIn(top) shouldBe Some(many(0))
    topEffects shouldBe Nil
  }

  "A list picker with a query" should "move the query's caret with Home and End, leaving the highlight" in {
    val (state, effects) = after(typed("et") ++ List(ModalLineStart, InsertChar('b'), ModalLineEnd, InsertChar('a')))

    query(state) shouldBe Some(TextField("beta", 4))
    highlightedIn(state) shouldBe Some(beta)
    effects shouldBe List(previewEffect(beta))

    val (homed, homedEffects) = after(typed("a") :+ ModalLineStart)
    query(homed) shouldBe Some(TextField("a", 0))
    highlightedIn(homed) shouldBe Some(alpha)
    homedEffects shouldBe Nil
  }

  it should "jump to its first and last rows with Ctrl+Home and Ctrl+End, and page its rows, previewing each" in {
    val (last, lastEffects) = after(List(ModalLast))
    highlightedIn(last) shouldBe Some(gamma)
    lastEffects shouldBe List(previewEffect(gamma))

    val (first, firstEffects) = after(List(ModalLast, ModalFirst))
    highlightedIn(first) shouldBe Some(alpha)
    firstEffects shouldBe List(previewEffect(gamma), previewEffect(alpha))

    val (paged, pagedEffects) = after(List(ModalPage(1)))
    highlightedIn(paged) shouldBe Some(gamma)
    pagedEffects shouldBe List(previewEffect(gamma))
  }

  "A floating list picker" should "receive Home, End, Ctrl+Home, Ctrl+End, PageUp and PageDown as modal keys" in {
    val translator = FocusedInputTranslator.forState(stateWith(filterable))
    val ctrl       = Set(Modifier.Ctrl)

    translator.translate(KeyStrokeInfo(InputKey.Home, None, Set.empty)) shouldBe ModalLineStart
    translator.translate(KeyStrokeInfo(InputKey.End, None, Set.empty)) shouldBe ModalLineEnd
    translator.translate(KeyStrokeInfo(InputKey.Home, None, ctrl)) shouldBe ModalFirst
    translator.translate(KeyStrokeInfo(InputKey.End, None, ctrl)) shouldBe ModalLast
    translator.translate(KeyStrokeInfo(InputKey.PageUp, None, Set.empty)) shouldBe ModalPage(-1)
    translator.translate(KeyStrokeInfo(InputKey.PageDown, None, Set.empty)) shouldBe ModalPage(1)
  }

  "A pending list picker" should "ignore Home, End and paging" in {
    val waiting = alpha.copy(waitingLabel = Some("Loading Alpha…"))
    val pending = ListPicker.filterable("Pick", Vector(waiting, beta)).copy(pending = Some(waiting))

    val (state, effects) = after(List(ModalLineEnd, ModalLast, ModalPage(1)), pending)
    state shouldBe stateWith(pending)
    effects shouldBe Nil
  }
