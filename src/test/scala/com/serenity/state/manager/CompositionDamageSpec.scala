package com.serenity.state.manager

import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A pre-edit is drawn into the buffer's text, so the buffer repaints when one begins, changes or ends (#2008). */
class CompositionDamageSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(1)

  private val buffer  = Buffer.fromString(bufferId, "hello\nworld")
  private val initial = AppState.initial
  private val base    = initial.copy(persisted = initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def stateWith(composition: Option[Composition]): AppState =
    base.copy(runtime = base.runtime.copy(composition = composition))

  private def composing(text: String): Composition =
    Composition(
      PaneId(0),
      bufferId,
      CursorPosition(0, 2),
      0L,
      text,
      List(CompositionClause(0, text.length, selected = true)),
      None
    )

  private def damagesEveryRow(before: AppState, after: AppState): Boolean =
    Damage.rowFactsFor(bufferId, DamageProducer.forTransition(before, after)).everyRow

  "DamageProducer" should "damage the buffer when a pre-edit begins" in {
    damagesEveryRow(stateWith(None), stateWith(Some(composing("か")))) shouldBe true
  }

  it should "damage the buffer when the pre-edit changes" in {
    damagesEveryRow(stateWith(Some(composing("か"))), stateWith(Some(composing("かん")))) shouldBe true
  }

  it should "damage the buffer when the pre-edit ends" in {
    damagesEveryRow(stateWith(Some(composing("か"))), stateWith(None)) shouldBe true
  }

  it should "damage nothing while the pre-edit stays as it was" in {
    val state = stateWith(Some(composing("か")))

    damagesEveryRow(state, state) shouldBe false
    DamageProducer.forTransition(state, state) shouldBe Damage.Nothing
  }
