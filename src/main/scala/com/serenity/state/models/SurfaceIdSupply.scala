package com.serenity.state.models

/** The counter behind every freshly *allocated* [[SurfaceId]] (`AppState.allocateSurfaceId`), distinct from a bare
  * `Int` so the allocation itself -- rendering `"surface-N"` and advancing the counter -- lives in one place instead of
  * being re-derived at each call site (issue #1693). Not every `SurfaceId` goes through this supply: some are fixed
  * string-literal constants (`SurfaceId.CursorPeek`, a docked panel's persisted id, ...) that never touch the counter
  * -- see `AppStateValidation`'s note on why a numeric coincidence there isn't evidence of a bug.
  */
opaque type SurfaceIdSupply = Int

object SurfaceIdSupply:
  def apply(value: Int): SurfaceIdSupply = value

  val initial: SurfaceIdSupply = SurfaceIdSupply(0)

  extension (supply: SurfaceIdSupply)
    def value: Int = supply

    /** The next id this supply would hand out, and the supply advanced past it. */
    def next: (SurfaceIdSupply, SurfaceId) = (SurfaceIdSupply(supply + 1), SurfaceId(s"surface-$supply"))

    /** Reserves at least as much of the counter as `other` already claims -- used when adopting a set of panels whose
      * own ids (parsed back out by `SessionLayout.nextSurfaceId`) may already be ahead of this supply, so a subsequent
      * allocation can never collide with one of them.
      */
    def reserveAtLeast(other: SurfaceIdSupply): SurfaceIdSupply = SurfaceIdSupply(supply.max(other))
