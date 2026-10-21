package com.serenity

import java.util.concurrent.{Executor, Executors}

import scala.concurrent.ExecutionContext

import cats.effect.unsafe.{IORuntime, IORuntimeConfig, Scheduler}
import cats.syntax.all.*
import com.serenity.config.AppConfig
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The store's mutual exclusion must not depend on which pool thread runs each blocking step (#2350). Each step of
  * `withExclusiveMutationLock` is its own `IO.blocking`, so a lock owned by a thread is only sound when every step
  * lands on the thread that took it.
  */
class UiPresetStoreLockOwnershipSpec extends AnyFlatSpec with Matchers:

  private def preset(name: String): UiPreset = UiPreset(name, AppConfig.default, Some(Theme.dark.name), Nil)

  private def runtimeOver[A](blocking: Executor, shutdown: () => Unit)(use: IORuntime => A): A =
    val compute                = Executors.newFixedThreadPool(4)
    val (scheduler, stopTimer) = Scheduler.createDefaultScheduler()
    val runtime = IORuntime(
      ExecutionContext.fromExecutor(compute),
      ExecutionContext.fromExecutor(blocking),
      scheduler,
      () => (),
      IORuntimeConfig()
    )
    try use(runtime)
    finally
      stopTimer()
      compute.shutdownNow()
      shutdown()

  private def onFreshThreadPerBlockingStep[A](use: IORuntime => A): A =
    runtimeOver(task => new Thread(task).start(), () => ())(use)

  private def onOneBlockingThread[A](use: IORuntime => A): A =
    val single = Executors.newSingleThreadExecutor()
    runtimeOver(
      single,
      () =>
        val _ = single.shutdownNow()
    )(use)

  "UiPresetStore" should "save when every blocking step of the lock runs on a different thread" in
    TestTemp.within("ui-preset-lock-fresh-threads") { dir =>
      val store = UiPresetStore(dir.resolve("ui-presets.json"))

      val result =
        onFreshThreadPerBlockingStep(runtime => store.upsert(preset("Focus")).attempt.unsafeRunSync()(using runtime))

      result shouldBe Right(())
    }

  it should "keep two stores on one file apart when every blocking step runs on one thread" in
    (1 to 10).foreach { attempt =>
      TestTemp.within(s"ui-preset-lock-one-thread-$attempt") { dir =>
        val path   = dir.resolve("ui-presets.json")
        val storeA = UiPresetStore(path)
        val storeB = UiPresetStore(path)

        val results = onOneBlockingThread { runtime =>
          (storeA.upsert(preset("Alpha")).attempt, storeB.upsert(preset("Beta")).attempt).parTupled
            .unsafeRunSync()(using runtime)
        }

        results shouldBe (Right(()), Right(()))
        onOneBlockingThread(runtime => storeA.load().unsafeRunSync()(using runtime)).names should contain allOf (
          "Alpha",
          "Beta"
        )
      }
    }
