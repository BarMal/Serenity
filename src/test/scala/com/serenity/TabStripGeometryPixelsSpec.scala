package com.serenity

import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2228: the tab strip appears with the second open buffer and goes with it, shifting every pane row down or up. A
  * frame drawn over the persisting image must equal a frame drawn from scratch, and its present must not be bounded to
  * a region that leaves the shifted rows out.
  */
class TabStripGeometryPixelsSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private val otherId = BufferId(2)

  private val single = stateOf(codeLines.mkString("\n"), CursorPosition(10, 0))

  private val withOtherTab: AppState =
    val other = Buffer.fromString(otherId, "second")
    single.copy(persisted =
      single.persisted.copy(
        buffers = single.persisted.buffers.updated(otherId, other),
        bufferOrder = single.persisted.bufferOrder :+ otherId
      )
    )

  private def showsSameAsCold(from: AppState, to: AppState): Unit =
    val session    = Session()
    val _          = session.settle(from)
    val (drawn, _) = session.draw(to, DamageProducer.forTransition(from, to))
    withClue("pixels differing from a cold render: ")(differingPixels(drawn, cold(to)) shouldBe empty)
    withClue("present bounded to a region although the pane rows moved: ")(session.repaintRegion shouldBe None)

  "A frame after the tab strip appears" should "equal a frame drawn from scratch" in
    showsSameAsCold(single, withOtherTab)

  "A frame after the tab strip goes away" should "equal a frame drawn from scratch" in
    showsSameAsCold(withOtherTab, single)

  private val showingOtherTab: AppState =
    withOtherTab.copy(persisted =
      withOtherTab.persisted.copy(layout =
        withOtherTab.persisted.layout
          .copy(editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, otherId)))
      )
    )

  "A frame after a second buffer opens in the pane" should "equal a frame drawn from scratch" in
    showsSameAsCold(single, showingOtherTab)

  "A frame after the second buffer closes in the pane" should "equal a frame drawn from scratch" in
    showsSameAsCold(showingOtherTab, single)
