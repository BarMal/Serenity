package com.serenity.state.manager

import java.util.concurrent.CountDownLatch

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.rope.{Balance, Rope}
import com.serenity.spellcheck.DictionaryLoader
import com.serenity.state.models.*
import com.serenity.testkit.AwaitCondition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Spell-check analysis is background work: a dictionary parse still running when the writer quits must not hold the
  * quit up. A parse cannot be interrupted, so cancelling the analysis lane must not wait for it.
  */
class StateManagerAnalysisShutdownSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private val prose: AppState =
    val initial = AppState.initial
    initial.copy(persisted =
      initial.persisted.copy(
        config = AppConfig.default.withSpellCheck(SpellCheckConfig(enabled = true)),
        buffers = initial.persisted.buffers.updated(
          bufferId,
          initial.persisted
            .buffers(bufferId)
            .copy(document = initial.persisted.buffers(bufferId).document.copy(content = Rope("hello world")))
        )
      )
    )

  "Shutting down while a dictionary parse is running" should "not wait for the parse" in {
    val parseStarted = CountDownLatch(1)
    val parseHeld    = CountDownLatch(1)
    val program =
      for
        modelRef <- ModelViews.modelOf(prose)
        operations <- StateManagerOperationBoundary.create(
          modelRef,
          NoOpLogger.impl[IO],
          loadDictionary = (config, cache) =>
            parseStarted.countDown()
            parseHeld.await()
            DictionaryLoader.loadSnapshot(config, cache)
        )
        _       <- operations.modelCommit.commitState(prose.copy(), prose)
        _       <- AwaitCondition.awaitValue(IO.blocking(parseStarted.getCount))(_ == 0L)
        outcome <- operations.shutdownEffects().timeout(2.seconds).attempt
        _       <- IO.blocking(parseHeld.countDown())
      yield outcome

    program.unsafeRunSync() shouldBe Right(())
  }
