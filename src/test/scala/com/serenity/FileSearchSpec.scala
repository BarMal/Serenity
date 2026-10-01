package com.serenity

import java.nio.file.Path

import com.serenity.command.{CommandRegistry, NavigationCommands}
import com.serenity.keystroke.events.*
import com.serenity.rope.{Balance, Leaf, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, AppEventReducer, ModalEventReducer, ModalStateReducer, SurfaceEffect}
import com.serenity.ui.widget.{Loadable, TextField}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** "Search in Open Files": a [[ListPicker]] over the lines of every open buffer that contain its query, loaded a batch
  * at a time, each picking the command that goes to that line.
  */
class FileSearchSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def withBuffers(buffers: (BufferId, Option[String], Rope)*): AppState =
    val base = AppState.initial
    val opened = buffers.map {
      case (id, name, content) =>
        val buffer = Buffer.newEmpty(id)
        id -> buffer.copy(document = buffer.document.copy(content = content, filePath = name.map(Path.of(_))))
    }
    base.copy(persisted = base.persisted.copy(buffers = opened.toMap, bufferOrder = opened.map(_._1).toList))

  private def opened(state: AppState): AppState =
    ModalStateReducer.show(Modal.ListPicker(BufferTextSearch.picker), state).state

  private def after(state: AppState, events: Seq[Event]): (AppState, List[AppEffect]) =
    events.foldLeft((state, List.empty[AppEffect])) {
      case ((current, effects), event) =>
        val result = ModalEventReducer.reduce(ModalType.ListPicker, event, current)
        (result.state, effects ++ result.effects)
    }

  private def typed(text: String): Seq[Event] = text.map(InsertChar(_))

  private def shown(state: AppState): ListPicker =
    state.modalSurface
      .map(_.content)
      .collect { case SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)) => picker }
      .getOrElse(fail("Expected an open list picker"))

  private def choices(state: AppState): Vector[ListChoice] =
    shown(state).items.toOption.map(_.items).getOrElse(fail(s"Expected loaded choices, got ${shown(state).items}"))

  private def goTo(bufferId: Int, line: Int) = NavigationCommands.goToBufferLine(BufferId(bufferId), line)

  private def numbered(count: Int): Rope = Rope((0 until count).map(line => s"needle result $line").mkString("\n"))

  "The FileSearch hotkey" should "ask the effect layer to open file search" in {
    val result = AppEventReducer.reduce(FileSearch, AppState.initial, CommandRegistry.default)

    result.effects shouldBe List(AppEffect.Surface(SurfaceEffect.OpenFileSearch))
  }

  "Search in Open Files" should "open with an empty query, inviting the user to type" in {
    BufferTextSearch.picker.title shouldBe "Search in Open Files"
    BufferTextSearch.picker.query shouldBe Some(TextField())
    BufferTextSearch.picker.items shouldBe Loadable.Empty("Type to search open files")
    BufferTextSearch.picker.hasMore shouldBe false
  }

  it should "list every matching line of every open buffer, in buffer order, ignoring case" in {
    val state = withBuffers(
      (BufferId(2), Some("/tmp/util.scala"), Rope("  def Helper()\nval x = 1")),
      (BufferId(1), Some("/tmp/main.scala"), Rope("object Main\n\tdef foo(x: Int)"))
    )

    val (searched, effects) = after(opened(state), typed("DEF"))

    choices(searched) shouldBe Vector(
      ListChoice("main.scala:2", Some("def foo(x: Int)"), goTo(1, 1)),
      ListChoice("util.scala:1", Some("def Helper()"), goTo(2, 0))
    )
    shown(searched).selectedChoice.map(_.label) shouldBe Some("main.scala:2")
    effects shouldBe Nil
  }

  it should "name an unsaved buffer by its id" in {
    val (searched, _) = after(opened(withBuffers((BufferId(4), None, Rope("draft line")))), typed("draft"))

    choices(searched).map(_.label) shouldBe Vector("buffer-4:1")
  }

  it should "say there are no matches, and invite typing again once the query is cleared" in {
    val (unmatched, _) = after(opened(withBuffers((BufferId(0), None, Rope("alpha")))), typed("zz"))
    shown(unmatched).items shouldBe Loadable.Empty("No matches")

    val (cleared, _) = after(unmatched, List(DeleteBackward, DeleteBackward))
    shown(cleared).items shouldBe Loadable.Empty("Type to search open files")
  }

  it should "go to the picked line, closing itself, when Enter is pressed" in {
    val state             = withBuffers((BufferId(0), Some("/tmp/a.txt"), Rope("one\ntwo\nthree")))
    val (picked, effects) = after(opened(state), typed("t") ++ List(MoveDown, Enter))

    picked.modalSurface shouldBe None
    effects shouldBe List(AppEffect.ExecuteCommand(goTo(0, 2)))
  }

  it should "load only the first batch of matches, without reading the whole buffer, and say more are available" in {
    val state = withBuffers((BufferId(0), Some("/tmp/many.txt"), GuardedRope(numbered(200))))

    val (searched, _) = after(opened(state), typed("needle"))

    choices(searched).size shouldBe 100
    choices(searched).lastOption.map(_.label) shouldBe Some("many.txt:100")
    shown(searched).hasMore shouldBe true
  }

  it should "load the next batch on Down at the last loaded match, highlighting its first match" in {
    val state          = withBuffers((BufferId(0), Some("/tmp/many.txt"), GuardedRope(numbered(250))))
    val (searched, _)  = after(opened(state), typed("needle") :+ MoveUp)
    val (extended, _)  = after(searched, List(MoveDown))
    val (exhausted, _) = after(extended, List.fill(101)(MoveUp) :+ MoveDown)

    shown(searched).selectedChoice.map(_.label) shouldBe Some("many.txt:100")
    choices(extended).size shouldBe 200
    shown(extended).selectedChoice.map(_.label) shouldBe Some("many.txt:101")
    shown(extended).hasMore shouldBe true

    choices(exhausted).size shouldBe 250
    shown(exhausted).selectedChoice.map(_.label) shouldBe Some("many.txt:201")
    shown(exhausted).hasMore shouldBe false
  }

  it should "wrap from the last match to the first once every match is loaded" in {
    val state        = withBuffers((BufferId(0), None, numbered(3)))
    val (last, _)    = after(opened(state), typed("needle") :+ MoveUp)
    val (wrapped, _) = after(last, List(MoveDown))

    shown(last).selectedChoice.map(_.label) shouldBe Some("buffer-0:3")
    shown(wrapped).selectedChoice.map(_.label) shouldBe Some("buffer-0:1")
  }

  it should "resume a batch mid-buffer and carry on into the next buffer" in {
    val state = withBuffers((BufferId(0), None, numbered(3)), (BufferId(1), None, numbered(2)))
    val small = BufferTextSearch.picker.copy(source = Some(PickerSource.BufferText(batchSize = 2)))
    val open  = ModalStateReducer.show(Modal.ListPicker(small), state).state

    val (first, _) = after(open, typed("needle"))
    choices(first).map(_.label) shouldBe Vector("buffer-0:1", "buffer-0:2")

    val (second, _) = after(first, List(MoveDown, MoveDown))
    choices(second).map(_.label) shouldBe Vector("buffer-0:1", "buffer-0:2", "buffer-0:3", "buffer-1:1")
    shown(second).selectedChoice.map(_.label) shouldBe Some("buffer-0:3")

    val (third, _) = after(second, List(MoveDown, MoveDown))
    choices(third).map(_.label) shouldBe
      Vector("buffer-0:1", "buffer-0:2", "buffer-0:3", "buffer-1:1", "buffer-1:2")
    shown(third).hasMore shouldBe false
  }

  it should "restart from the first match when the query changes after paging" in {
    val state         = withBuffers((BufferId(0), None, numbered(250)))
    val (paged, _)    = after(opened(state), typed("needle") ++ List(MoveUp, MoveDown))
    val (narrowed, _) = after(paged, typed(" result 1"))

    choices(narrowed).map(_.label).take(3) shouldBe Vector("buffer-0:2", "buffer-0:11", "buffer-0:12")
    shown(narrowed).selectedChoice.map(_.label) shouldBe Some("buffer-0:2")
    shown(narrowed).hasMore shouldBe true
  }

  /** A rope whose whole text can't be read: the search must stream lines rather than materialise a buffer. `Rope` is
    * sealed, so this extends the still-open `Leaf`, built from the real text, and only refuses `collect`.
    */
  final private class GuardedRope(delegate: Rope) extends Leaf(delegate.collect()):
    override def collect(): String = throw AssertionError("file search should not materialise the whole buffer")
end FileSearchSpec
