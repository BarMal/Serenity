package com.serenity.state.models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #1693: `Runtime.nextSurfaceId` used to be a bare `Int`, with the `"surface-N"` rendering and increment
  * re-derived at each call site (`AppState.allocateSurfaceId`, `UiPreset.applyToState`). `SurfaceIdSupply` now owns
  * that logic itself.
  */
class SurfaceIdSupplySpec extends AnyFlatSpec with Matchers:

  "initial" should "start at 0" in {
    SurfaceIdSupply.initial.value shouldBe 0
  }

  "next" should "hand out \"surface-N\" for the supply's current value, then advance past it" in {
    val (advanced, id) = SurfaceIdSupply(3).next

    id shouldBe SurfaceId("surface-3")
    advanced.value shouldBe 4
  }

  it should "allocate distinct, increasing ids across repeated calls" in {
    val (afterFirst, first)   = SurfaceIdSupply.initial.next
    val (afterSecond, second) = afterFirst.next

    first should not be second
    afterSecond.value shouldBe 2
  }

  "reserveAtLeast" should "keep this supply's own value when it is already ahead" in {
    SurfaceIdSupply(5).reserveAtLeast(SurfaceIdSupply(2)) shouldBe SurfaceIdSupply(5)
  }

  it should "adopt the other supply's value when it is ahead of this one" in {
    SurfaceIdSupply(2).reserveAtLeast(SurfaceIdSupply(5)) shouldBe SurfaceIdSupply(5)
  }
