package com.serenity.state.components

import com.serenity.command.{Command, CommandIntent, EditIntent}
import com.serenity.keystroke.events.{Enter, Escape, InsertChar, MoveDown, MoveToEnd, MoveUp}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContextMenuComponentSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val cut   = Command.typed("cut", "Cut", CommandIntent.Edit(EditIntent.Cut), label = "Cut")
  private val copy  = Command.typed("copy", "Copy", CommandIntent.Edit(EditIntent.Copy), label = "Copy")
  private val paste = Command.typed("paste", "Paste", CommandIntent.Edit(EditIntent.Paste), label = "Paste")

  private val menuId = SurfaceId("context-menu")
  private val editor = Focus.EditorPane(PaneId(0))

  private val open: AppState =
    val menu = ContextMenu(
      "Edit",
      editor,
      List(cut, copy, paste).map(command => ContextMenuItem(command.name, command.name, command))
    )
    val surface = UiSurface(
      menuId,
      SurfaceContent.ContextMenu(menu),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    AppState.initial
      .copy(runtime = AppState.initial.runtime.copy(uiSurfaces = List(surface)))
      .pushFocus(Focus.Surface(menuId))

  private val component = ContextMenuComponent()

  private def highlighted(state: AppState): Option[Int] =
    state.contextMenuSurface.map(_.content).collect { case SurfaceContent.ContextMenu(menu) => menu.selectedIndex }

  private def applied(result: ComponentResult, state: AppState): AppState =
    result match
      case ComponentResult.StateChange(update) => update(state)
      case other                               => fail(s"expected a state change, got $other")

  "The context menu" should "move its highlight with the arrow keys, wrapping, and jump to the last item" in {
    highlighted(applied(component.processEvent(MoveDown, open), open)) shouldBe Some(1)
    highlighted(applied(component.processEvent(MoveUp, open), open)) shouldBe Some(2)
    highlighted(applied(component.processEvent(MoveToEnd, open), open)) shouldBe Some(2)
  }

  it should "run the highlighted command on Enter, closing itself and returning focus to its target" in {
    val onCopy = applied(component.processEvent(MoveDown, open), open)

    component.processEvent(Enter, onCopy) match
      case ComponentResult.Composite(List(ComponentResult.StateChange(close), ComponentResult.ExecuteCommand(run))) =>
        run shouldBe copy
        val closed = close(onCopy)
        closed.contextMenuSurface shouldBe None
        closed.persisted.focus shouldBe editor
      case other => fail(s"expected close-then-run, got $other")
  }

  it should "close on Escape, handing focus back" in {
    val closed = applied(component.processEvent(Escape, open), open)
    closed.contextMenuSurface shouldBe None
    closed.persisted.focus shouldBe editor
  }

  it should "stay open when an unrelated key is typed" in {
    component.processEvent(InsertChar('x'), open) shouldBe ComponentResult.NoChange
  }
end ContextMenuComponentSpec
