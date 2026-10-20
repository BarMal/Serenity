package com.serenity.state.manager

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.rope.{Balance, Rope}
import com.serenity.spellcheck.IncrementalSpellFixture
import com.serenity.state.models.*
import com.serenity.testkit.AwaitCondition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Each analysis pass used to look for the dictionary files again, though they change far less often than the text. */
class StateManagerDictionarySnapshotReuseSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private def prose(text: String): AppState =
    val initial = AppState.initial
    val buffer  = initial.persisted.buffers(bufferId)
    initial.copy(persisted =
      initial.persisted.copy(
        config = AppConfig.default.withSpellCheck(SpellCheckConfig(enabled = true)),
        buffers = initial.persisted.buffers.updated(
          bufferId,
          buffer.copy(document = buffer.document.copy(content = Rope(text)))
        )
      )
    )

  private def typedInto(state: AppState, text: String): AppState =
    val buffer = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(bufferId, buffer.copy(document = buffer.document.withContent(Rope(text))))
      )
    )

  private def flaggedCount(operations: StateManagerOperationBoundary): IO[Int] =
    operations.modelCommit.currentState.map(
      _.runtime.languageService.diagnosticsState.diagnostics.values.map(_.size).sum
    )

  "Successive analysis passes" should "load the dictionary once while its files are not reported changed" in {
    val loads      = AtomicInteger(0)
    val first      = prose("the cat sat")
    val typed      = typedInto(first, "the cat sat zzzz")
    val typedAgain = typedInto(typed, "the cat sat zzzz wurld")
    val program =
      for
        modelRef <- ModelViews.modelOf(first)
        operations <- StateManagerOperationBoundary.create(
          modelRef,
          NoOpLogger.impl[IO],
          loadDictionary = (_, _) =>
            loads.incrementAndGet()
            IncrementalSpellFixture.snapshot
        )
        _ <- operations.modelCommit.commitState(first.copy(), first)
        _ <- operations.modelCommit.commitState(typed, first)
        _ <- AwaitCondition.awaitValue(flaggedCount(operations))(_ == 1)
        _ <- operations.modelCommit.commitState(typedAgain, typed)
        _ <- AwaitCondition.awaitValue(flaggedCount(operations))(_ == 2)
        _ <- operations.shutdownEffects()
      yield loads.get

    program.unsafeRunSync() shouldBe 1
  }

  it should "load the dictionary again after its files are reported changed" in {
    val loads      = AtomicInteger(0)
    val first      = prose("the cat sat")
    val typed      = typedInto(first, "the cat sat zzzz")
    val typedAgain = typedInto(typed, "the cat sat zzzz wurld")
    val program =
      for
        modelRef <- ModelViews.modelOf(first)
        operations <- StateManagerOperationBoundary.create(
          modelRef,
          NoOpLogger.impl[IO],
          loadDictionary = (_, _) =>
            loads.incrementAndGet()
            IncrementalSpellFixture.snapshot
        )
        _ <- operations.modelCommit.commitState(first.copy(), first)
        _ <- operations.modelCommit.commitState(typed, first)
        _ <- AwaitCondition.awaitValue(flaggedCount(operations))(_ == 1)
        _ <- operations.refreshDictionaryFingerprints()
        _ <- operations.modelCommit.commitState(typedAgain, typed)
        _ <- AwaitCondition.awaitValue(flaggedCount(operations))(_ == 2)
        _ <- operations.shutdownEffects()
      yield loads.get

    program.unsafeRunSync() shouldBe 2
  }

  it should "load the dictionary again when the spell-check config changes" in {
    val loads = AtomicInteger(0)
    val first = prose("the cat sat zzzz")
    val reconfigured = first.copy(persisted =
      first.persisted.copy(config =
        first.persisted.config.withSpellCheck(
          first.persisted.config.languageToolsConfig.spellCheck.copy(additionalWords = List("zzzz"))
        )
      )
    )
    val program =
      for
        modelRef <- ModelViews.modelOf(first)
        operations <- StateManagerOperationBoundary.create(
          modelRef,
          NoOpLogger.impl[IO],
          loadDictionary = (_, _) =>
            loads.incrementAndGet()
            IncrementalSpellFixture.snapshot
        )
        _ <- operations.modelCommit.commitState(first.copy(), first)
        _ <- AwaitCondition.awaitValue(flaggedCount(operations))(_ == 1)
        _ <- operations.modelCommit.commitState(reconfigured, first)
        _ <- AwaitCondition.awaitValue(IO(loads.get))(_ == 2)
        _ <- operations.shutdownEffects()
      yield loads.get

    program.unsafeRunSync() shouldBe 2
  }
