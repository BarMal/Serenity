package com.serenity.state.manager

import com.serenity.TestWorkspaceTrees
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, MotionAccessibility}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Selection grow/settle (issue #1085 phase 3): `CursorViewport.ensureVisibleCursors` seeds `Cursor.selectionGeometry`
  * whenever a cursor's own selection changes -- extend, shrink, create or clear -- gated by the `SelectionGeometry`
  * motion family (including accessibility), independently for every live cursor, and *not* GUI-only the way caret glide
  * is (`SelectionGeometryState`'s column-granular model serves TUI's cell painting too). Mirrors
  * `CursorViewportGlideSpec`, the equivalent seeding spec for caret glide.
  */
class CursorViewportSelectionGeometrySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWith(
    buffer: Buffer,
    config: AppConfig => AppConfig = identity,
    isTuiMode: Boolean = false
  ): AppState =
    val base = AppState.initial
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = base.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId),
        config = config(base.persisted.config)
      ),
      runtime = base.runtime.copy(isTuiMode = isTuiMode)
    )

  private def bufferWith(cursors: Cursor*): Buffer =
    val lineCount = (cursors.map(_.position.line).maxOption.getOrElse(0) + 1).max(5)
    val content   = (0 until lineCount).map(i => s"line $i has some text").mkString("\n")
    Buffer
      .fromString(bufferId, content)
      .copy(
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 20),
        editing = EditingState.fromCursors(cursors.toList)
      )

  private def withCursors(buffer: Buffer, cursors: Cursor*): Buffer =
    buffer.copy(editing = EditingState.fromCursors(cursors.toList))

  "CursorViewport.ensureVisibleCursors" should "seed a selection geometry when a selection is created" in {
    val before = stateWith(bufferWith(Cursor(CursorPosition(0, 0))))
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        Map(
          bufferId -> withCursors(
            before.persisted.buffers(bufferId),
            Cursor(CursorPosition(0, 5), selectionAnchor = Some(CursorPosition(0, 0)))
          )
        )
      )
    )

    val result = CursorViewport.ensureVisibleCursors(before, after)

    val geometry = result.persisted.buffers(bufferId).editing.cursors.head.selectionGeometry
    geometry shouldBe defined
    geometry.get.rectFor(0, 0).map(_.width) shouldBe Some(0)
  }

  it should "not seed a selection geometry when the selection is unchanged" in {
    val before = stateWith(
      bufferWith(Cursor(CursorPosition(0, 5), selectionAnchor = Some(CursorPosition(0, 0))))
    )
    val after = before

    val result = CursorViewport.ensureVisibleCursors(before, after)

    result.persisted.buffers(bufferId).editing.cursors.head.selectionGeometry shouldBe None
  }

  it should "not seed a selection geometry when the SelectionGeometry motion family is disabled by accessibility" in {
    val before = stateWith(
      bufferWith(Cursor(CursorPosition(0, 0))),
      config = _.withMotionAccessibility(MotionAccessibility.Off)
    )
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        Map(
          bufferId -> withCursors(
            before.persisted.buffers(bufferId),
            Cursor(CursorPosition(0, 5), selectionAnchor = Some(CursorPosition(0, 0)))
          )
        )
      )
    )

    val result = CursorViewport.ensureVisibleCursors(before, after)

    result.persisted.buffers(bufferId).editing.cursors.head.selectionGeometry shouldBe None
  }

  it should "seed a selection geometry in TUI mode too, unlike caret glide" in {
    val before = stateWith(bufferWith(Cursor(CursorPosition(0, 0))), isTuiMode = true)
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        Map(
          bufferId -> withCursors(
            before.persisted.buffers(bufferId),
            Cursor(CursorPosition(0, 5), selectionAnchor = Some(CursorPosition(0, 0)))
          )
        )
      )
    )

    val result = CursorViewport.ensureVisibleCursors(before, after)

    result.persisted.buffers(bufferId).editing.cursors.head.selectionGeometry shouldBe defined
  }

  it should "retarget an in-flight geometry rather than reseeding it when the selection extends again" in {
    val before = stateWith(bufferWith(Cursor(CursorPosition(0, 0))))
    val afterFirstExtend = before.copy(persisted =
      before.persisted.copy(buffers =
        Map(
          bufferId -> withCursors(
            before.persisted.buffers(bufferId),
            Cursor(CursorPosition(0, 3), selectionAnchor = Some(CursorPosition(0, 0)))
          )
        )
      )
    )
    val firstResult = CursorViewport.ensureVisibleCursors(before, afterFirstExtend)
    val firstGeometry =
      firstResult.persisted
        .buffers(bufferId)
        .editing
        .cursors
        .head
        .selectionGeometry
        .getOrElse(fail("expected geometry"))
    val midFlight = firstGeometry.advance
    midFlight.isComplete shouldBe false
    val midFlightRect = midFlight.rectFor(0, 0).getOrElse(fail("expected a rect"))

    val midFlightBuffer = firstResult.persisted
      .buffers(bufferId)
      .withCursorList(
        firstResult.persisted.buffers(bufferId).editing.cursors.map(_.copy(selectionGeometry = Some(midFlight)))
      )
    val midFlightState =
      firstResult.copy(persisted = firstResult.persisted.copy(buffers = Map(bufferId -> midFlightBuffer)))

    val afterSecondExtend = midFlightState.copy(persisted =
      midFlightState.persisted.copy(buffers =
        Map(
          bufferId -> withCursors(
            midFlightBuffer,
            Cursor(CursorPosition(0, 8), selectionAnchor = Some(CursorPosition(0, 0)))
          )
        )
      )
    )
    val secondResult = CursorViewport.ensureVisibleCursors(midFlightState, afterSecondExtend)

    val retargeted = secondResult.persisted
      .buffers(bufferId)
      .editing
      .cursors
      .head
      .selectionGeometry
      .getOrElse(fail("expected a retargeted geometry"))
    val retargetedTween = retargeted.lines.find(_.key == SelectionLineKey(0, 0)).getOrElse(fail("expected a line"))
    retargetedTween.tween.start shouldBe midFlightRect
    retargetedTween.tween.currentFrame shouldBe 0
  }

  it should "seed each cursor's selection geometry independently in a multi-cursor buffer" in {
    val before = stateWith(bufferWith(Cursor(CursorPosition(0, 0)), Cursor(CursorPosition(1, 0))))
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        Map(
          bufferId -> withCursors(
            before.persisted.buffers(bufferId),
            Cursor(CursorPosition(0, 5), selectionAnchor = Some(CursorPosition(0, 0))),
            Cursor(CursorPosition(1, 0))
          )
        )
      )
    )

    val result  = CursorViewport.ensureVisibleCursors(before, after)
    val cursors = result.persisted.buffers(bufferId).editing.cursors.toList

    cursors(0).selectionGeometry shouldBe defined
    cursors(1).selectionGeometry shouldBe None
  }
