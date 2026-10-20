package com.serenity.state.manager

import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a transition tells the renderer about a scrollbar: a held thumb is painted differently, and says so as damage
  * of its own rather than as a repaint of the pane.
  */
class ScrollbarDamageSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val before = AppState.initial

  private def holding(state: AppState, grab: Option[ScrollbarGrab]): AppState =
    state.copy(runtime = state.runtime.copy(pointerGesture = state.runtime.pointerGesture.copy(scrollbarGrab = grab)))

  "Taking hold of a thumb" should "damage that pane's scrollbar and nothing else" in {
    val after = holding(before, Some(ScrollbarGrab(PaneId(0), 12)))

    DamageProducer.forTransition(before, after) shouldBe Damage.Scrollbar(PaneId(0))
  }

  "Letting go of a thumb" should "damage that pane's scrollbar" in {
    val held = holding(before, Some(ScrollbarGrab(PaneId(0), 12)))

    DamageProducer.forTransition(held, holding(held, None)) shouldBe Damage.Scrollbar(PaneId(0))
  }

  "Dragging a thumb" should "damage no scrollbar, since the position it moves to is damage enough" in {
    val held   = holding(before, Some(ScrollbarGrab(PaneId(0), 12)))
    val buffer = held.persisted.buffers.values.headOption.getOrElse(fail("expected a buffer"))
    val moved = held.copy(persisted =
      held.persisted.copy(buffers =
        held.persisted.buffers.updated(buffer.id, buffer.copy(viewport = buffer.viewport.scrolledTo(7, 0, 0)))
      )
    )

    val damage = DamageProducer.forTransition(held, moved)

    Damage.scrollbarPaneIds(damage) shouldBe empty
    damage shouldBe Damage.BufferScrolled(buffer.id)
  }

  "A transition that leaves the thumb alone" should "damage no scrollbar" in {
    DamageProducer.forTransition(before, before) shouldBe Damage.Nothing
  }

  "A scrollbar's damage" should "name its pane among the facts it is combined with" in {
    val combined = cats.Monoid[Damage].combine(Damage.Scrollbar(PaneId(1)), Damage.Chrome)

    Damage.scrollbarPaneIds(combined) shouldBe Set(PaneId(1))
    Damage.touchesChrome(combined) shouldBe true
  }

  it should "let a bounded repaint stand, since the bar's rect is known" in {
    Damage.isBufferRowsOrChromeOnly(Damage.Scrollbar(PaneId(0))) shouldBe true
    Damage.isBufferRowsOnly(Damage.Scrollbar(PaneId(0))) shouldBe false
  }
