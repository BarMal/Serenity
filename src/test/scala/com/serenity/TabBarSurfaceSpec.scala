package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The always-visible tab strip surface (issue #1074 epic, #1075-1077): derived each frame from `bufferOrder`, the same
  * "no separate open-tabs state to keep in sync" trade-off `AppState.floatingStatusLineSurface` and
  * `TabListContent.build` already make. Only appears once 2+ buffers are open -- with a single buffer there is nothing
  * to switch between (issue #1074 decision).
  */
class TabBarSurfaceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def withSecondBuffer(state: AppState): AppState =
    val second = Buffer.fromString(BufferId(1), "second")
    state.copy(persisted =
      state.persisted.copy(
        buffers = state.persisted.buffers + (second.id -> second),
        bufferOrder = state.persisted.bufferOrder :+ second.id
      )
    )

  "AppState.tabBarSurface" should "be absent with a single buffer open" in {
    AppState.initial.tabBarSurface shouldBe None // one buffer, by construction
  }

  it should "carry one TabListEntry per open buffer, in bufferOrder, once 2+ buffers are open" in {
    val state = withSecondBuffer(AppState.initial)

    val surface = state.tabBarSurface.getOrElse(fail("expected a tab bar surface"))

    surface.id shouldBe UiSurface.TabBarSurfaceId
    surface.content shouldBe SurfaceContent.TabBar(
      entries = TabListContent.build(state).entries,
      activeBufferId = state.focusedBufferId
    )
  }

  it should "be reachable through surfaceById, like the other derived-per-frame surfaces" in {
    val state = withSecondBuffer(AppState.initial)

    state.surfaceById(UiSurface.TabBarSurfaceId) shouldBe state.tabBarSurface
  }

  it should "go back to absent once a session drops back to a single buffer" in {
    val twoBuffers = withSecondBuffer(AppState.initial)
    val backToOne = twoBuffers.copy(persisted =
      twoBuffers.persisted.copy(
        buffers = twoBuffers.persisted.buffers - BufferId(1),
        bufferOrder = twoBuffers.persisted.bufferOrder.filterNot(_ == BufferId(1))
      )
    )

    backToOne.tabBarSurface shouldBe None
  }
