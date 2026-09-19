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

/** The status line's segment list is ordered and independently toggleable: the include/exclude toggle and the discrete
  * reorder intents (mirroring `MovePanelEarlier`/`Later`'s own settings-menu shape), plus the rendered text.
  */
class StatusLineSegmentsSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  /** A state manager whose status line starts empty, so each test adds exactly the segments it reasons about. */
  private def makeStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StatusLineSegmentsSpec"))
    val sm     = StateManager.apply(logger).unsafeRunSync()
    StatusSegment.values.foreach(segment => execute(sm, StatusLineIntent.SetSegmentIncluded(segment, included = false)))
    sm

  private def execute(sm: StateManager, intent: StatusLineIntent): Unit =
    sm.commandExecutor
      .executeCommand(
        Command.typed(
          "test-status-intent",
          "test",
          CommandIntent.Settings(SettingsIntent.StatusLine(intent)),
          CommandCategory.Settings,
          label = "test"
        )
      )
      .unsafeRunSync()

  private def segments(sm: StateManager): List[StatusSegment] =
    sm.getCurrentState.unsafeRunSync().persisted.config.statusLine.segments

  "AppConfig" should "show position, language, title and mode on a pinned row by default" in {
    AppConfig.default.statusLine shouldBe StatusLineConfig(
      List(StatusSegment.Position, StatusSegment.Language, StatusSegment.Title, StatusSegment.Mode),
      StatusLinePlacement.Pinned
    )
  }

  "SetSegmentIncluded" should "order included segments by their canonical definition order, not toggle order" in {
    val sm = makeStateManager()
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Title, included = true))
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Position, included = true))

    segments(sm) shouldBe List(StatusSegment.Position, StatusSegment.Title)
  }

  it should "restore a re-enabled segment to its canonical position rather than appending it (#1533)" in {
    val sm = makeStateManager()
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Position, included = true))
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Title, included = true))
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.WordCount, included = true))
    // Toggle a middle segment off, then back on -- it must return to the middle, not jump to the end.
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Title, included = false))
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Title, included = true))

    segments(sm) shouldBe List(StatusSegment.Position, StatusSegment.Title, StatusSegment.WordCount)
  }

  it should "be idempotent when the segment is already included" in {
    val sm = makeStateManager()
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Position, included = true))
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Position, included = true))

    segments(sm) shouldBe List(StatusSegment.Position)
  }

  it should "remove the segment from the list when included = false" in {
    val sm = makeStateManager()
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Position, included = true))
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Title, included = true))
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Position, included = false))

    segments(sm) shouldBe List(StatusSegment.Title)
  }

  "MoveSegmentEarlier" should "swap a segment with its predecessor" in {
    val sm = makeStateManager()
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Position, included = true))
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Title, included = true))
    execute(sm, StatusLineIntent.MoveSegmentEarlier(StatusSegment.Title))

    segments(sm) shouldBe List(StatusSegment.Title, StatusSegment.Position)
  }

  it should "leave the list unchanged when the segment is already first" in {
    val sm = makeStateManager()
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Position, included = true))
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Title, included = true))
    execute(sm, StatusLineIntent.MoveSegmentEarlier(StatusSegment.Position))

    segments(sm) shouldBe List(StatusSegment.Position, StatusSegment.Title)
  }

  "MoveSegmentLater" should "swap a segment with its successor" in {
    val sm = makeStateManager()
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Position, included = true))
    execute(sm, StatusLineIntent.SetSegmentIncluded(StatusSegment.Title, included = true))
    execute(sm, StatusLineIntent.MoveSegmentLater(StatusSegment.Position))

    segments(sm) shouldBe List(StatusSegment.Title, StatusSegment.Position)
  }

  "SetPlacement and ToggleVisibility" should "move the line between pinned, floating and off" in {
    val sm        = makeStateManager()
    def placement = sm.getCurrentState.unsafeRunSync().persisted.config.statusLine.placement

    execute(sm, StatusLineIntent.SetPlacement(StatusLinePlacement.Floating))
    placement shouldBe StatusLinePlacement.Floating
    execute(sm, StatusLineIntent.ToggleVisibility)
    placement shouldBe StatusLinePlacement.Off
    execute(sm, StatusLineIntent.ToggleVisibility)
    placement shouldBe StatusLinePlacement.Pinned
  }

  "AppState.statusLineText" should "join included segments in the configured order" in {
    import com.serenity.state.models.*

    val bufferId = BufferId(1)
    val paneId   = PaneId(0)
    val buffer = Buffer
      .fromString(bufferId, "hello world")
      .copy(editing = EditingState(List(CursorPosition(0, 3))))
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.EditorPane(paneId),
        config = AppState.initial.persisted.config.withStatusLineSegments(
          List(StatusSegment.WordCount, StatusSegment.Position)
        )
      )
    )

    state.statusLineText shouldBe Some("2 words | Line 1, Col 4")
  }
