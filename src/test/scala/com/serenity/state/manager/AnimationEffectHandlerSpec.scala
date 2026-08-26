package com.serenity.state.manager

import java.awt.Color

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.animation.{AnimatedCell, AnimationOwner, AnimationState, CharacterKey, TextEdit}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.BufferId
import com.serenity.state.reducers.AnimationEffect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnimationEffectHandlerSpec extends AnyFlatSpec with Matchers:

  private given Balance = Balance.default

  private val bufferId = BufferId(0)

  private def handlerWith(
    initial: Map[BufferId, AnimationState]
  ): (AnimationEffectHandler, Ref[IO, Map[BufferId, AnimationState]]) =
    val ref = Ref.of[IO, Map[BufferId, AnimationState]](initial).unsafeRunSync()
    (new AnimationEffectHandler(ref), ref)

  "AnimationEffectHandler" should "merge a delta into a buffer with no prior animations" in {
    val (handler, ref) = handlerWith(Map.empty)
    val cell           = AnimatedCell.completed('a', Color.WHITE)

    handler.interpret(AnimationEffect.Merge(bufferId, Map(CharacterKey(0, 0) -> cell))).unsafeRunSync()

    ref.get.unsafeRunSync().apply(bufferId).getCell(0, 0) shouldBe Some(cell)
  }

  it should "remap an existing buffer's animations through edits" in {
    val state          = AnimationState.empty.addCharacterAnimation('a', 3, 1, Color.BLACK, Color.WHITE, 5)
    val (handler, ref) = handlerWith(Map(bufferId -> state))
    val before         = Rope("line one\nline two")
    val after          = Rope("\nline one\nline two")

    handler
      .interpret(AnimationEffect.RemapThroughEdits(bufferId, before, after, List(TextEdit(0, 0, "\n"))))
      .unsafeRunSync()

    ref.get.unsafeRunSync().apply(bufferId).getCell(3, 2) shouldBe defined
  }

  it should "no-op a remap for a buffer with no tracked animations" in {
    val (handler, ref) = handlerWith(Map.empty)
    val rope           = Rope("abc")

    handler
      .interpret(AnimationEffect.RemapThroughEdits(bufferId, rope, rope, List(TextEdit(0, 0, "x"))))
      .unsafeRunSync()

    ref.get.unsafeRunSync() shouldBe empty
  }

  it should "clear all animations for a buffer" in {
    val state          = AnimationState.empty.addCharacterAnimation('a', 0, 0, Color.BLACK, Color.WHITE, 5)
    val (handler, ref) = handlerWith(Map(bufferId -> state))

    handler.interpret(AnimationEffect.ClearAll(bufferId)).unsafeRunSync()

    ref.get.unsafeRunSync() shouldBe empty
  }

  it should "clear only animations owned by the given family" in {
    val editorCell = AnimatedCell.parametricForeground('a', Color.BLACK, Color.WHITE, 5)
    val uiCell =
      AnimatedCell.parametricForeground('b', Color.BLACK, Color.WHITE, 5).copy(owner = AnimationOwner.UiTransitions)
    val state          = AnimationState(Map(CharacterKey(0, 0) -> editorCell, CharacterKey(1, 0) -> uiCell))
    val (handler, ref) = handlerWith(Map(bufferId -> state))

    handler.interpret(AnimationEffect.ClearOwner(bufferId, AnimationOwner.EditorText)).unsafeRunSync()

    val remaining = ref.get.unsafeRunSync().apply(bufferId)
    remaining.getCell(0, 0) shouldBe empty
    remaining.getCell(1, 0) shouldBe defined
  }
