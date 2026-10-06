package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Marking a buffer `FollowCaret` and resolving it later must leave the viewport exactly where placing it eagerly would
  * have, in every layout the editor places a caret in.
  */
class ViewportResolutionSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  given Balance = Balance.default

  private val paneId  = PaneId(0)
  private val typedId = BufferId(1)
  private val otherId = BufferId(2)

  private val wrapped    = AppConfig.default.withWordWrap(true)
  private val typewriter = wrapped.withSurfaceConfig(wrapped.surfaceConfig.copy(typewriterScrollingEnabled = true))
  private val unwrapped  = AppConfig.default.withWordWrap(false)
  private val columns    = wrapped.withColumnMode(true)

  private val layouts: List[(String, AppConfig)] =
    List("wrapped" -> wrapped, "typewriter" -> typewriter, "unwrapped" -> unwrapped, "column mode" -> columns)

  private val words = Vector("lorem", "ipsum", "dolor", "sit", "amet", "consectetur", "adipiscing", "elit", "sed")

  final private case class Scenario(
      lines: Vector[String],
      viewport: Viewport,
      from: CursorPosition,
      to: CursorPosition,
      tui: Boolean
  )

  private def cursorIn(lines: Vector[String]): Gen[CursorPosition] =
    for
      line   <- Gen.choose(0, lines.size - 1)
      column <- Gen.choose(0, lines(line).length)
    yield CursorPosition(line, column)

  private val scenarios: Gen[Scenario] =
    for
      lineCount <- Gen.choose(1, 120)
      lines <- Gen.listOfN(
        lineCount,
        Gen.choose(0, 40).flatMap(count => Gen.listOfN(count, Gen.oneOf(words)).map(_.mkString(" ")))
      )
      visibleLines   <- Gen.choose(4, 40)
      visibleColumns <- Gen.choose(20, 120)
      topLine        <- Gen.choose(0, lineCount - 1)
      leftColumn     <- Gen.choose(0, 12)
      from           <- cursorIn(lines.toVector)
      to             <- cursorIn(lines.toVector)
      tui            <- Gen.oneOf(true, false)
    yield Scenario(
      lines.toVector,
      Viewport(topLine, leftColumn, visibleLines, visibleColumns),
      from,
      to,
      tui
    )

  private def stateFor(scenario: Scenario, config: AppConfig, cursor: CursorPosition): AppState =
    val typed = Buffer
      .fromString(typedId, scenario.lines.mkString("\n"))
      .copy(viewport = scenario.viewport, editing = EditingState(List(cursor)))
    val other = Buffer
      .fromString(otherId, "untouched\nbuffer")
      .copy(
        viewport = Viewport(visibleLines = 10, visibleColumns = 40),
        editing = EditingState(List(CursorPosition(1, 3)))
      )
    val base = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        config = config,
        buffers = Map(typedId -> typed, otherId -> other),
        bufferOrder = List(typedId, otherId),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, typedId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )
    if scenario.tui then base.copy(runtime = base.runtime.copy(capabilities = FrontendCapabilities.tui())) else base

  private def movedTo(state: AppState, cursor: CursorPosition): AppState =
    val buffer = state.persisted.buffers(typedId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(typedId, buffer.copy(editing = EditingState(List(cursor))))
      )
    )

  private def viewportOf(state: AppState, id: BufferId): Viewport = state.persisted.buffers(id).viewport

  layouts.foreach { (name, config) =>
    property(s"resolving a marked move places the viewport as ensureVisibleCursors does in $name layout") {
      forAll(scenarios, minSuccessful(40)) { scenario =>
        val before = stateFor(scenario, config, scenario.from)
        val after  = movedTo(before, scenario.to)

        ViewportResolution.resolve(ViewportResolution.markFollow(before, after)) shouldBe
          CursorViewport.ensureVisibleCursors(before, after)
      }
    }
  }

  property("markFollow marks the buffer whose head moved and leaves the others as they were") {
    forAll(scenarios, minSuccessful(20)) { scenario =>
      val before = stateFor(scenario, wrapped, scenario.from)
      val after  = movedTo(before, scenario.to)
      val marked = ViewportResolution.markFollow(before, after)

      if scenario.from == scenario.to then marked should be theSameInstanceAs after
      else viewportOf(marked, typedId).placement shouldBe ViewportPlacement.FollowCaret
      marked.persisted.buffers(otherId) should be theSameInstanceAs after.persisted.buffers(otherId)
    }
  }

  property("a buffer already following the caret keeps following it when its head does not move") {
    forAll(scenarios, minSuccessful(20)) { scenario =>
      val before = stateFor(scenario, wrapped, scenario.from)
      val moved  = ViewportResolution.markFollow(before, movedTo(before, scenario.to))
      val again  = ViewportResolution.markFollow(moved, moved)

      again should be theSameInstanceAs moved
    }
  }

  property("resolving leaves every buffer placed, and a state with nothing to place as the same instance") {
    forAll(scenarios, minSuccessful(20)) { scenario =>
      val before   = stateFor(scenario, wrapped, scenario.from)
      val resolved = ViewportResolution.resolve(ViewportResolution.markFollow(before, movedTo(before, scenario.to)))

      resolved.persisted.buffers.values.map(_.viewport.placement).toSet shouldBe Set(ViewportPlacement.Placed)
      ViewportResolution.resolve(resolved) should be theSameInstanceAs resolved
    }
  }
