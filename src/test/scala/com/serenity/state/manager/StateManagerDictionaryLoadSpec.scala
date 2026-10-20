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

/** Typing never waits on the spell-check dictionary: its diagnostics arrive when the parse finishes. */
class StateManagerDictionaryLoadSpec extends AnyFlatSpec with Matchers:

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

  "A commit that needs spell-check analysis" should "complete while the dictionary is still being parsed" in {
    val parseStarted = CountDownLatch(1)
    val parseHeld    = CountDownLatch(1)
    val first        = prose("hello world")
    val typed        = prose("hello world again")
    val program =
      for
        modelRef <- ModelViews.modelOf(first)
        operations <- StateManagerOperationBoundary.create(
          modelRef,
          NoOpLogger.impl[IO],
          loadDictionary = (config, cache) =>
            parseStarted.countDown()
            parseHeld.await()
            DictionaryLoader.loadSnapshot(config, cache)
        )
        _       <- operations.modelCommit.commitState(first.copy(), first)
        _       <- AwaitCondition.awaitValue(IO.blocking(parseStarted.getCount))(_ == 0L)
        outcome <- operations.modelCommit.commitState(first, typed).timeout(2.seconds).attempt
        _       <- IO.blocking(parseHeld.countDown())
        _       <- operations.shutdownEffects().timeout(30.seconds)
      yield outcome

    program.unsafeRunSync() shouldBe Right(())
  }
