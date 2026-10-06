package com.serenity

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.AppRuntime
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import fs2.concurrent.SignallingRef
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Commits that don't come from input -- diagnostics, async opens, task output -- must still reach the screen. */
class AppRuntimeBackgroundCommitSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(1)

  private def withBuffer(text: String): AppState =
    val state = AppState.initial
    state.copy(persisted =
      state.persisted.copy(buffers = Map(bufferId -> Buffer.fromString(bufferId, text)), bufferOrder = List(bufferId))
    )

  "AppRuntime.wakeRenderLoopOnCommit" should "emit the damage a commit caused" in {
    val program = for
      emitted <- Ref.of[IO, Vector[Damage]](Vector.empty)
      wake = AppRuntime.wakeRenderLoopOnCommit(damage => emitted.update(_ :+ damage))
      _      <- wake(withBuffer("before"), withBuffer("after"))
      result <- emitted.get
    yield result

    val emitted = program.unsafeRunSync()
    emitted should have size 1
    emitted.headOption should not be Some(Damage.Nothing)
  }

  it should "stay silent for a commit that changes nothing on screen" in {
    val state = withBuffer("same")
    val program = for
      emitted <- Ref.of[IO, Vector[Damage]](Vector.empty)
      wake = AppRuntime.wakeRenderLoopOnCommit(damage => emitted.update(_ :+ damage))
      _      <- wake(state, state)
      result <- emitted.get
    yield result

    program.unsafeRunSync() shouldBe empty
  }

  "StateManager commits" should "be reported to the registered commit observer" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val program = for
      stateManager <- StateManager.apply(logger, dictionaryCache = SharedDictionary.default)
      observed     <- Ref.of[IO, Vector[(Boolean, Boolean)]](Vector.empty)
      _ <- stateManager.runtimeLifecycle.observeCommits((before, after) =>
        observed.update(_ :+ (before.runtime.chapterGhostsVisible -> after.runtime.chapterGhostsVisible))
      )
      _ <- stateManager.updateStateValidated(state =>
        state.copy(runtime = state.runtime.copy(chapterGhostsVisible = false))
      )
      result <- observed.get
    yield result

    program.unsafeRunSync() shouldBe Vector(true -> false)
  }

  "AppRuntime.followFrameTimingSetting" should "publish frame timing turning on or off, and nothing else" in {
    val off = AppState.initial
    val on  = AppState.initial(off.persisted.config.withFrameTiming(true))

    val program = for
      enabled <- SignallingRef.of[IO, Boolean](false)
      follow = AppRuntime.followFrameTimingSetting(enabled)
      _         <- follow(off, on)
      turnedOn  <- enabled.get
      _         <- enabled.set(false)
      _         <- follow(on, on)
      unchanged <- enabled.get
      _         <- enabled.set(true)
      _         <- follow(on, off)
      turnedOff <- enabled.get
    yield (turnedOn, unchanged, turnedOff)

    program.unsafeRunSync() shouldBe ((true, false, false))
  }

  "AppRuntime.followLatencyTraceSetting" should "publish the latency trace turning on or off, and nothing else" in {
    val off = AppState.initial
    val on  = AppState.initial(off.persisted.config.withLatencyTrace(true))

    val program = for
      enabled <- SignallingRef.of[IO, Boolean](false)
      follow = AppRuntime.followLatencyTraceSetting(enabled)
      _         <- follow(off, on)
      turnedOn  <- enabled.get
      _         <- enabled.set(false)
      _         <- follow(on, on)
      unchanged <- enabled.get
      _         <- enabled.set(true)
      _         <- follow(on, off)
      turnedOff <- enabled.get
    yield (turnedOn, unchanged, turnedOff)

    program.unsafeRunSync() shouldBe ((true, false, false))
  }
