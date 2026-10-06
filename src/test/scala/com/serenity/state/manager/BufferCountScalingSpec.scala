package com.serenity.state.manager

import java.lang.management.ManagementFactory
import java.nio.file.Path

import com.serenity.app.AppRuntime
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.ui.accessibility.AccessibilitySync
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Per-commit work must not grow with the number of open buffers (memory research finding 15). Each commit-time check
  * is measured on one edit to one buffer, with 1 and with 200 other clean buffers open, by the bytes the measuring
  * thread allocates: a walk over every buffer that builds a set, a copy or an entry tuple per buffer shows up as a
  * ratio far above 1, where a check that skips untouched buffers allocates the same either way.
  */
class BufferCountScalingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val threadMemory = ManagementFactory.getThreadMXBean match
    case sun: com.sun.management.ThreadMXBean => Some(sun)
    case _                                    => None

  private def allocated(): Long =
    threadMemory.fold(0L)(_.getThreadAllocatedBytes(Thread.currentThread.threadId))

  private def bytesPerRun(runs: Int)(operation: => Boolean): Long =
    (0 until 30000).foreach(_ => operation)
    val before = allocated()
    (0 until runs).foreach(_ => operation)
    (allocated() - before) / runs

  private val EditedId = BufferId(1)

  private def stateWith(openBuffers: Int, editedText: String): AppState =
    val buffers = (1 to openBuffers).map { index =>
      val id = BufferId(index)
      id -> Buffer.fromFile(id, Path.of(s"/tmp/scaling/file$index.md"), if id == EditedId then editedText else "text")
    }.toMap
    val initial = AppState.initial
    initial.copy(persisted =
      initial.persisted.copy(
        buffers = buffers,
        bufferOrder = buffers.keys.toList,
        layout = initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), EditedId)),
          activeEditorPaneId = Some(PaneId(0))
        ),
        focus = Focus.EditorPane(PaneId(0))
      )
    )

  private def edited(state: AppState): AppState =
    val buffer  = state.persisted.buffers(EditedId)
    val changed = buffer.copy(document = buffer.document.withContent(Rope("text, edited")))
    state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(EditedId, changed)))

  private def assertFlat(label: String)(check: (AppState, AppState) => Boolean): Unit =
    assume(threadMemory.isDefined, "thread allocation counters unavailable")
    val one      = stateWith(1, "text")
    val many     = stateWith(200, "text")
    val oneEdit  = edited(one)
    val manyEdit = edited(many)
    val small    = bytesPerRun(2000)(check(one, oneEdit))
    val large    = bytesPerRun(2000)(check(many, manyEdit))
    info(f"[SCALING] $label%-36s 1 buffer=$small%,8d B/op  200 buffers=$large%,8d B/op")
    large.toDouble should be < (small.toDouble * 2.0 + 64.0)

  "watchInputsChanged" should "allocate no more per edit with 200 open buffers than with 1" in
    assertFlat("watchInputsChanged")(AppRuntime.watchInputsChanged)

  it should "still announce a buffer gaining a path, a buffer closing and a buffer swapped for another" in {
    val base    = stateWith(3, "text")
    val buffers = base.persisted.buffers
    def withBuffers(next: Map[BufferId, Buffer]) =
      base.copy(persisted = base.persisted.copy(buffers = next))

    val renamed = buffers.updated(BufferId(2), Buffer.fromFile(BufferId(2), Path.of("/tmp/scaling/other.md"), "text"))
    val closed  = buffers - BufferId(2)
    val swapped = (buffers - BufferId(2)).updated(BufferId(9), Buffer.fromFile(BufferId(9), Path.of("/x/y.md"), "t"))

    AppRuntime.watchInputsChanged(base, withBuffers(renamed)) shouldBe true
    AppRuntime.watchInputsChanged(base, withBuffers(closed)) shouldBe true
    AppRuntime.watchInputsChanged(base, withBuffers(swapped)) shouldBe true
    AppRuntime.watchInputsChanged(base, withBuffers(buffers)) shouldBe false
  }

  "EditIdleSessionSave.due" should "allocate no more per edit with 200 open buffers than with 1" in
    assertFlat("EditIdleSessionSave.due")(EditIdleSessionSave.due)

  it should "find an edited unsaved buffer however many other buffers are open" in {
    val many   = stateWith(200, "text")
    val buffer = many.persisted.buffers(BufferId(150))
    val dirty = many.copy(persisted =
      many.persisted.copy(buffers =
        many.persisted.buffers
          .updated(BufferId(150), buffer.copy(document = buffer.document.withContent(Rope("more"))))
      )
    )

    EditIdleSessionSave.due(many, dirty) shouldBe true
    EditIdleSessionSave.due(many, many) shouldBe false
  }

  "AccessibilitySync" should "judge an edit's relevance with no more allocation per edit with 200 open buffers than 1" in
    assertFlat("AccessibilitySync.accessiblyEqual")((before, after) => AccessibilitySync.accessiblyEqual(before, after))

  it should "treat a preview-generation bump on one of many buffers as irrelevant but a text change as relevant" in {
    val many   = stateWith(200, "text")
    val buffer = many.persisted.buffers(BufferId(77))
    val bumped = many.copy(persisted =
      many.persisted.copy(buffers =
        many.persisted.buffers.updated(BufferId(77), buffer.copy(markdownPreviewEditGeneration = 5L))
      )
    )

    AccessibilitySync.accessiblyEqual(many, bumped) shouldBe true
    AccessibilitySync.accessiblyEqual(many, edited(many)) shouldBe false
  }

  "withBufferIndexesRefreshed" should "allocate no more per edit with 200 open buffers than with 1" in
    assertFlat("withBufferIndexesRefreshed")((_, after) => after.withBufferIndexesRefreshed ne null)
