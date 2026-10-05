package com.serenity.state.models

import cats.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DamageSpec extends AnyFlatSpec with Matchers:

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  "Damage.isBufferRowsOnly" should "hold for Nothing" in {
    Damage.isBufferRowsOnly(Damage.Nothing) shouldBe true
  }

  it should "hold for a bare BufferRows" in {
    Damage.isBufferRowsOnly(Damage.BufferRows(bufferId, Set(0, 1))) shouldBe true
  }

  it should "hold for a bare BufferCells" in {
    Damage.isBufferRowsOnly(Damage.BufferCells(bufferId, 0, 0, Some(5))) shouldBe true
  }

  it should "hold for a combination of only BufferRows and BufferCells across different buffers" in {
    val damage = (Damage.BufferRows(bufferId, Set(0)): Damage) |+| Damage.BufferCells(BufferId(1), 2, 0, None)
    Damage.isBufferRowsOnly(damage) shouldBe true
  }

  it should "not hold for Everything" in {
    Damage.isBufferRowsOnly(Damage.Everything) shouldBe false
  }

  it should "not hold for Chrome" in {
    Damage.isBufferRowsOnly(Damage.Chrome) shouldBe false
  }

  it should "not hold for PaneChrome" in {
    Damage.isBufferRowsOnly(Damage.PaneChrome(paneId)) shouldBe false
  }

  it should "not hold for Surface" in {
    Damage.isBufferRowsOnly(Damage.Surface(SurfaceId("overlay"))) shouldBe false
  }

  it should "not hold when a BufferRows is combined with a PaneChrome fact" in {
    val damage = (Damage.BufferRows(bufferId, Set(0)): Damage) |+| Damage.PaneChrome(paneId)
    Damage.isBufferRowsOnly(damage) shouldBe false
  }

  "Damage.isEverything" should "hold when Everything is combined with row-level facts" in {
    val damage = (Damage.BufferRows(bufferId, Set(0)): Damage) |+| Damage.Everything
    Damage.isEverything(damage) shouldBe true
  }

  "Damage.coarsenToRows" should "ignore facts about other buffers" in {
    val damage = (Damage.BufferRows(bufferId, Set(1, 2)): Damage) |+| Damage.BufferRows(BufferId(1), Set(9))
    Damage.coarsenToRows(bufferId, damage) shouldBe Set(1, 2)
  }

  "Damage.surfaceIds" should "be empty for Nothing" in {
    Damage.surfaceIds(Damage.Nothing) shouldBe Set.empty
  }

  it should "be empty for Everything (handled separately via isEverything)" in {
    Damage.surfaceIds(Damage.Everything) shouldBe Set.empty
  }

  it should "collect a bare Surface fact" in {
    Damage.surfaceIds(Damage.Surface(SurfaceId("overlay"))) shouldBe Set(SurfaceId("overlay"))
  }

  it should "collect every Surface fact out of a Combined damage, ignoring unrelated facts" in {
    val damage =
      (Damage.Surface(SurfaceId("a")): Damage) |+| Damage.Surface(SurfaceId("b")) |+| Damage.BufferRows(
        bufferId,
        Set(0)
      )
    Damage.surfaceIds(damage) shouldBe Set(SurfaceId("a"), SurfaceId("b"))
  }

  "Damage.BufferAll" should "absorb row and cell facts about the same buffer only" in {
    val other = BufferId(1)
    val damage =
      (Damage.BufferAll(bufferId): Damage) |+| Damage.BufferRows(bufferId, Set(3)) |+|
        Damage.BufferCells(bufferId, 4, 0, Some(1)) |+| Damage.BufferRows(other, Set(3))
    damage shouldBe Damage.Combined(Set(Damage.BufferAll(bufferId), Damage.BufferRows(other, Set(3))))
    Damage.damagesEveryRow(bufferId, damage) shouldBe true
    Damage.damagesEveryRow(other, damage) shouldBe false
  }

  it should "count as buffer-row damage for a bounded repaint" in {
    Damage.isBufferRowsOnly(Damage.BufferAll(bufferId)) shouldBe true
  }

  "Damage.BufferCells" should "keep two disjoint spans on one row apart instead of covering everything between" in {
    val damage = (Damage.BufferCells(bufferId, 0, 2, Some(3)): Damage) |+| Damage.BufferCells(bufferId, 0, 40, Some(41))
    Damage.damagedSpans(bufferId, damage) shouldBe
      Set(Damage.BufferCells(bufferId, 0, 2, Some(3)), Damage.BufferCells(bufferId, 0, 40, Some(41)))
  }

  it should "merge touching spans on one row" in {
    val damage = (Damage.BufferCells(bufferId, 0, 2, Some(3)): Damage) |+| Damage.BufferCells(bufferId, 0, 3, Some(4))
    damage shouldBe Damage.BufferCells(bufferId, 0, 2, Some(4))
  }

  "Damage.isBufferRowsOrChromeOnly" should "hold for row facts with Chrome, which has a rect of its own" in {
    Damage.isBufferRowsOrChromeOnly((Damage.BufferRows(bufferId, Set(0)): Damage) |+| Damage.Chrome) shouldBe true
  }

  it should "not hold for PaneChrome, Surface or Everything" in {
    Damage.isBufferRowsOrChromeOnly(Damage.PaneChrome(paneId)) shouldBe false
    Damage.isBufferRowsOrChromeOnly(Damage.Surface(SurfaceId("overlay"))) shouldBe false
    Damage.isBufferRowsOrChromeOnly(Damage.Everything) shouldBe false
  }
