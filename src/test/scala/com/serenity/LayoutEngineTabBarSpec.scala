package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Split out of LayoutEngineSpec to keep that file under the architecture ratchet's file-length target. */
class LayoutEngineTabBarSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  behavior of "LayoutEngine tab bar reservation"

  it should "reserve no tab bar strip with a single buffer open" in {
    val state        = AppState.initial // one buffer, by construction
    val viewportSize = ViewportSize(100, 30)

    val calculatedLayout = LayoutEngine.calculateLayout(state, viewportSize)

    // Matches the pre-existing baseline for this exact state/viewport ("use the full workspace width..." in
    // LayoutEngineSpec): `AppConfig.default`'s pinned status line already reserves its own bottom gutter row
    // regardless of the tab bar, and line numbers are on by default -- neither of which this test is about, so the
    // reservation under test is "no *additional* row for the tab bar", not "no chrome at all".
    calculatedLayout.tabBarRect shouldBe None
    calculatedLayout.editorPanelRect shouldBe LayoutRect(3, 0, 97, 29)
  }

  it should "reserve a one-row strip at the top of the frame once 2+ buffers are open, shrinking the workspace beneath it by exactly that row" in {
    val secondBuffer      = Buffer.fromString(BufferId(1), "second")
    val singleBufferState = AppState.initial
    val twoBufferState = singleBufferState.copy(persisted =
      singleBufferState.persisted.copy(
        buffers = singleBufferState.persisted.buffers + (secondBuffer.id -> secondBuffer),
        bufferOrder = singleBufferState.persisted.bufferOrder :+ secondBuffer.id
      )
    )
    val viewportSize = ViewportSize(100, 30)

    val singleBufferLayout = LayoutEngine.calculateLayout(singleBufferState, viewportSize)
    val twoBufferLayout    = LayoutEngine.calculateLayout(twoBufferState, viewportSize)

    val tabBar = twoBufferLayout.tabBarRect.getOrElse(fail("expected a reserved tab bar rect"))
    tabBar shouldBe LayoutRect(0, 0, viewportSize.width, 1)

    // The reserved row shifts the whole workspace down by exactly one row rather than merely shrinking a total,
    // since it sits at the top of the frame (unlike the bottom gutter, nothing exists below it to absorb the shrink).
    twoBufferLayout.editorPanelRect.y shouldBe singleBufferLayout.editorPanelRect.y + 1
    twoBufferLayout.editorPanelRect.height shouldBe singleBufferLayout.editorPanelRect.height - 1
    twoBufferLayout.editorPanelRect.x shouldBe singleBufferLayout.editorPanelRect.x
    twoBufferLayout.editorPanelRect.width shouldBe singleBufferLayout.editorPanelRect.width
  }

  it should "revert to no reserved strip once a session drops back to a single buffer" in {
    val secondBuffer = Buffer.fromString(BufferId(1), "second")
    val base         = AppState.initial
    val twoBuffers = base.copy(persisted =
      base.persisted.copy(
        buffers = base.persisted.buffers + (secondBuffer.id -> secondBuffer),
        bufferOrder = base.persisted.bufferOrder :+ secondBuffer.id
      )
    )
    val backToOne = twoBuffers.copy(persisted =
      twoBuffers.persisted.copy(
        buffers = twoBuffers.persisted.buffers - secondBuffer.id,
        bufferOrder = twoBuffers.persisted.bufferOrder.filterNot(_ == secondBuffer.id)
      )
    )
    val viewportSize = ViewportSize(100, 30)

    LayoutEngine.calculateLayout(backToOne, viewportSize).tabBarRect shouldBe None
  }
