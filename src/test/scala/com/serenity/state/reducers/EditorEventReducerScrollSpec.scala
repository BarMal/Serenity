package com.serenity.state.reducers

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Horizontal scroll-gesture support (issue #1568): `ScrollLeft`/`ScrollRight` pan `leftColumn` the same way
  * `ScrollUp`/`ScrollDown` (covered end-to-end in `ScrollingNavigationSpec`) already pan `topLine`, or -- while column
  * mode and word wrap are both on -- reduce exactly as `ColumnLeft`/`ColumnRight` already do, so a scroll gesture goes
  * through the very same reduce path (and therefore the same `CursorViewport.seedColumnTransition` animated sweep at
  * the effect boundary) as the keyboard equivalent, rather than a separate ad hoc path.
  */
class EditorEventReducerScrollSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateWith(
    text: String,
    cursors: List[CursorPosition] = List(CursorPosition(0, 0)),
    viewport: Viewport = Viewport.default,
    config: AppConfig => AppConfig = identity
  ): AppState =
    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(editing = EditingStateFixtures(cursors = cursors), viewport = viewport)
    val base = AppState.initial
    base.copy(persisted =
      base.persisted.copy(buffers = Map(bufferId -> buffer), config = config(base.persisted.config))
    )

  private def bufferAfter(event: TextEntryEvent, state: AppState): Buffer =
    EditorEventReducer.reduce(event, paneId, state).state.persisted.buffers(bufferId)

  "ScrollRight" should "pan leftColumn forward in non-wrapped mode" in {
    val longLine = "x" * 300
    val before = stateWith(
      longLine,
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 1, visibleColumns = 80),
      config = _.withWordWrap(false)
    )

    bufferAfter(ScrollRight(20), before).viewport.leftColumn shouldBe 20
  }

  it should "clamp to the longest currently visible line, not scroll past all real content" in {
    val before = stateWith(
      s"${"x" * 100}\nshort",
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 2, visibleColumns = 80),
      config = _.withWordWrap(false)
    )

    bufferAfter(ScrollRight(1000), before).viewport.leftColumn shouldBe (100 - 80 + 1)
  }

  "ScrollLeft" should "pan leftColumn backward, clamped at the start of the line" in {
    val longLine = "x" * 300
    val before = stateWith(
      longLine,
      viewport = Viewport(topLine = 0, leftColumn = 30, visibleLines = 1, visibleColumns = 80),
      config = _.withWordWrap(false)
    )

    bufferAfter(ScrollLeft(10), before).viewport.leftColumn shouldBe 20
  }

  it should "not scroll past the start of the line" in {
    val longLine = "x" * 300
    val before = stateWith(
      longLine,
      viewport = Viewport(topLine = 0, leftColumn = 10, visibleLines = 1, visibleColumns = 80),
      config = _.withWordWrap(false)
    )

    bufferAfter(ScrollLeft(100), before).viewport.leftColumn shouldBe 0
  }

  "A horizontal scroll gesture" should "be a no-op under plain word wrap, where leftColumn is always pinned to 0" in {
    val before = stateWith("hello world", config = _.withWordWrap(true).withColumnMode(false))

    bufferAfter(ScrollRight(10), before).viewport.leftColumn shouldBe 0
  }

  it should "move the cursor forward exactly like ColumnRight while column mode is on" in {
    val manyLines = (0 until 200).map(i => s"line$i").mkString("\n")
    val before    = stateWith(manyLines, config = _.withWordWrap(true).withColumnMode(true))

    bufferAfter(ScrollRight(3), before).editing.cursors shouldBe bufferAfter(ColumnRight, before).editing.cursors
  }

  it should "move the cursor backward exactly like ColumnLeft while column mode is on" in {
    val manyLines = (0 until 200).map(i => s"line$i").mkString("\n")
    val before = stateWith(
      manyLines,
      cursors = List(CursorPosition(50, 0)),
      config = _.withWordWrap(true).withColumnMode(true)
    )

    bufferAfter(ScrollLeft(3), before).editing.cursors shouldBe bufferAfter(ColumnLeft, before).editing.cursors
  }
