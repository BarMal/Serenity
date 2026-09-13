package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** #1261: the cursor info bar's Off/Position/Detailed presets became an ordered, independently toggleable list of
  * segments. Covers the include/exclude toggle and discrete reorder intents (mirroring `MovePanelEarlier`/`Later`'s own
  * settings-menu shape) plus the settings-menu construction and the render-side formatting.
  */
class CursorInfoBarSegmentsSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def makeStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("CursorInfoBarSegmentsSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def execute(sm: StateManager, intent: CursorIntent): Unit =
    sm.commandExecutor
      .executeCommand(
        Command.typed(
          "test-cursor-intent",
          "test",
          CommandIntent.Settings(SettingsIntent.Cursor(intent)),
          CommandCategory.Settings,
          label = "test"
        )
      )
      .unsafeRunSync()

  "AppConfig" should "default cursorInfoBarSegments to empty" in {
    AppConfig.default.cursorInfoBarSegments shouldBe Nil
  }

  "SetCursorInfoBarSegmentIncluded" should "order included segments by their canonical definition order, not toggle order" in {
    val sm = makeStateManager()
    // Enabled in reverse of definition order (Position before Title); the list must still come back canonical.
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Position, included = true))
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Title, included = true))

    sm.getCurrentState.unsafeRunSync().persisted.config.cursorInfoBarSegments shouldBe
      List(CursorInfoBarSegment.Title, CursorInfoBarSegment.Position)
  }

  it should "restore a re-enabled segment to its canonical position rather than appending it (#1533)" in {
    val sm = makeStateManager()
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Title, included = true))
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Position, included = true))
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.WordCount, included = true))
    // Toggle a middle segment off, then back on -- it must return to the middle, not jump to the end.
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Position, included = false))
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Position, included = true))

    sm.getCurrentState.unsafeRunSync().persisted.config.cursorInfoBarSegments shouldBe
      List(CursorInfoBarSegment.Title, CursorInfoBarSegment.Position, CursorInfoBarSegment.WordCount)
  }

  it should "be idempotent when the segment is already included" in {
    val sm = makeStateManager()
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Position, included = true))
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Position, included = true))

    sm.getCurrentState.unsafeRunSync().persisted.config.cursorInfoBarSegments shouldBe
      List(CursorInfoBarSegment.Position)
  }

  it should "remove the segment from the list when included = false" in {
    val sm = makeStateManager()
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Position, included = true))
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Title, included = true))
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Position, included = false))

    sm.getCurrentState.unsafeRunSync().persisted.config.cursorInfoBarSegments shouldBe
      List(CursorInfoBarSegment.Title)
  }

  // Enabling Title + Position yields the canonical order [Title, Position]; the reorder intents move away from it.
  "MoveCursorInfoBarSegmentEarlier" should "swap a segment with its predecessor" in {
    val sm = makeStateManager()
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Title, included = true))
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Position, included = true))
    execute(sm, CursorIntent.MoveCursorInfoBarSegmentEarlier(CursorInfoBarSegment.Position))

    sm.getCurrentState.unsafeRunSync().persisted.config.cursorInfoBarSegments shouldBe
      List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title)
  }

  it should "leave the list unchanged when the segment is already first" in {
    val sm = makeStateManager()
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Title, included = true))
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Position, included = true))
    execute(sm, CursorIntent.MoveCursorInfoBarSegmentEarlier(CursorInfoBarSegment.Title))

    sm.getCurrentState.unsafeRunSync().persisted.config.cursorInfoBarSegments shouldBe
      List(CursorInfoBarSegment.Title, CursorInfoBarSegment.Position)
  }

  "MoveCursorInfoBarSegmentLater" should "swap a segment with its successor" in {
    val sm = makeStateManager()
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Title, included = true))
    execute(sm, CursorIntent.SetCursorInfoBarSegmentIncluded(CursorInfoBarSegment.Position, included = true))
    execute(sm, CursorIntent.MoveCursorInfoBarSegmentLater(CursorInfoBarSegment.Title))

    sm.getCurrentState.unsafeRunSync().persisted.config.cursorInfoBarSegments shouldBe
      List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title)
  }

  "AppState.cursorInfoBarText" should "join included segments in the configured order" in {
    import com.serenity.state.models.*

    val bufferId = BufferId(1)
    val paneId   = PaneId(0)
    val buffer = Buffer
      .fromString(bufferId, "hello world")
      .copy(editing = EditingState(cursors = List(CursorPosition(0, 3))))
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.EditorPane(paneId),
        config = AppState.initial.persisted.config.withCursorInfoBarSegments(
          List(CursorInfoBarSegment.WordCount, CursorInfoBarSegment.Position)
        )
      )
    )

    state.cursorInfoBarText shouldBe Some("2 words | Line 1, Col 4")
  }
