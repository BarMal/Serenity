package com.serenity.state.manager

import com.serenity.TestWorkspaceTrees
import com.serenity.animation.Tween
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, MotionAccessibility}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Caret-glide (issue #1085 phase 2): `CursorViewport.ensureVisibleCursors` seeds `Cursor.glide` whenever a cursor's
  * own position changes -- any change (typing, navigation, mouse click, search jump), not narrowly arrow-key
  * navigation -- gated by the `Cursor` motion family (including accessibility), GUI-canvas-only, and independently for
  * every live cursor. Mirrors `CursorViewportColumnTransitionSpec`, the equivalent seeding spec for column transitions.
  */
class CursorViewportGlideSpec extends AnyFlatSpec with Matchers:

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

  private def bufferAt(cursorPositions: CursorPosition*): Buffer =
    val lineCount = (cursorPositions.map(_.line).maxOption.getOrElse(0) + 1).max(5)
    val content   = (0 until lineCount).map(i => s"line $i has some text").mkString("\n")
    Buffer
      .fromString(bufferId, content)
      .copy(
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 20),
        editing = EditingState(cursorPositions.toList)
      )

  private def movedTo(buffer: Buffer, cursorPositions: CursorPosition*): Buffer =
    buffer.copy(editing = EditingState(cursorPositions.toList))

  "CursorViewport.ensureVisibleCursors" should "seed a glide when the cursor's position changes" in {
    val before = stateWith(bufferAt(CursorPosition(0, 0)))
    val after = before.copy(persisted =
      before.persisted.copy(buffers = Map(bufferId -> movedTo(before.persisted.buffers(bufferId), CursorPosition(0, 5))))
    )

    val result = CursorViewport.ensureVisibleCursors(before, after)

    val glide = result.persisted.buffers(bufferId).editing.cursors.head.glide
    glide shouldBe defined
    glide.get.progress shouldBe 0.0
    glide.get.end.xPx should be > glide.get.start.xPx
  }

  it should "not seed a glide when the cursor's position is unchanged" in {
    val before = stateWith(bufferAt(CursorPosition(0, 0)))
    val after  = before

    val result = CursorViewport.ensureVisibleCursors(before, after)

    result.persisted.buffers(bufferId).editing.cursors.head.glide shouldBe None
  }

  it should "not seed a glide when the Cursor motion family is disabled by accessibility" in {
    val before = stateWith(bufferAt(CursorPosition(0, 0)), config = _.withMotionAccessibility(MotionAccessibility.Off))
    val after = before.copy(persisted =
      before.persisted.copy(buffers = Map(bufferId -> movedTo(before.persisted.buffers(bufferId), CursorPosition(0, 5))))
    )

    val result = CursorViewport.ensureVisibleCursors(before, after)

    result.persisted.buffers(bufferId).editing.cursors.head.glide shouldBe None
  }

  it should "not seed a glide in TUI mode, where the caret snaps instantly" in {
    val before = stateWith(bufferAt(CursorPosition(0, 0)), isTuiMode = true)
    val after = before.copy(persisted =
      before.persisted.copy(buffers = Map(bufferId -> movedTo(before.persisted.buffers(bufferId), CursorPosition(0, 5))))
    )

    val result = CursorViewport.ensureVisibleCursors(before, after)

    result.persisted.buffers(bufferId).editing.cursors.head.glide shouldBe None
  }

  it should "retarget an in-flight glide rather than reseeding it at progress zero when the cursor moves again" in {
    val before = stateWith(bufferAt(CursorPosition(0, 0)))
    val afterFirstMove = before.copy(persisted =
      before.persisted.copy(buffers = Map(bufferId -> movedTo(before.persisted.buffers(bufferId), CursorPosition(0, 5))))
    )
    val firstResult = CursorViewport.ensureVisibleCursors(before, afterFirstMove)
    val firstGlide  = firstResult.persisted.buffers(bufferId).editing.cursors.head.glide.getOrElse(fail("expected a glide"))

    val midFlight = firstGlide.advance
    midFlight.isComplete shouldBe false
    val midFlightBuffer = firstResult.persisted.buffers(bufferId).withCursorList(
      firstResult.persisted.buffers(bufferId).editing.cursors.map(_.copy(glide = Some(midFlight)))
    )
    val midFlightState =
      firstResult.copy(persisted = firstResult.persisted.copy(buffers = Map(bufferId -> midFlightBuffer)))

    val afterSecondMove = midFlightState.copy(persisted =
      midFlightState.persisted.copy(buffers = Map(bufferId -> movedTo(midFlightBuffer, CursorPosition(0, 10))))
    )
    val secondResult = CursorViewport.ensureVisibleCursors(midFlightState, afterSecondMove)

    val retargeted =
      secondResult.persisted.buffers(bufferId).editing.cursors.head.glide.getOrElse(fail("expected a retargeted glide"))
    retargeted.start shouldBe midFlight.currentValue
    retargeted.currentFrame shouldBe 0
    retargeted.isComplete shouldBe false
  }

  it should "seed each cursor's glide independently in a multi-cursor buffer" in {
    val before = stateWith(bufferAt(CursorPosition(0, 0), CursorPosition(1, 0)))
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        Map(bufferId -> movedTo(before.persisted.buffers(bufferId), CursorPosition(0, 5), CursorPosition(1, 0)))
      )
    )

    val result  = CursorViewport.ensureVisibleCursors(before, after)
    val cursors = result.persisted.buffers(bufferId).editing.cursors.toList

    cursors(0).glide shouldBe defined
    cursors(1).glide shouldBe None
  }
