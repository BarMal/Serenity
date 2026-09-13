package com.serenity.state.manager

import java.nio.file.{Path, Paths}

import cats.effect.std.Semaphore
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.config.PreferredWindowSize
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Dedicated coverage for `StateManagerCapabilityPorts` (#1442): the file is almost entirely capability-port trait
  * signatures with no logic of their own to test, but [[EffectRuntimePort]] carries one real default method,
  * `trackRecentFile` -- de-duplicating and bounding a most-recently-used file list -- shared with the identical private
  * helper in [[StateManagerFilePersistence]]. This suite pins that shared behavior down directly against a minimal
  * `EffectRuntimePort` implementation, independent of any concrete port's other dependencies.
  */
class StateManagerCapabilityPortsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  /** The minimal `EffectRuntimePort` needed to reach its one default method -- every other member is a throwaway value,
    * never exercised by these tests.
    */
  private val port: EffectRuntimePort = new EffectRuntimePort:
    val stateRef                = Ref.of[IO, AppState](AppState.initial).unsafeRunSync()
    val themeNamesRef           = Ref.of[IO, List[String]](Nil).unsafeRunSync()
    val quitSignal              = Deferred[IO, Unit].unsafeRunSync()
    val logger                  = NoOpLogger.impl[IO]
    val themeManager            = AppThemeManager.create
    val lspQueue                = LspEffectQueue.create.unsafeRunSync()
    val projectTaskFiberRef     = Ref.of[IO, Option[ManagedProjectTask]](None).unsafeRunSync()
    val projectTaskSemaphore    = Semaphore[IO](1).unsafeRunSync()
    val onFontConfigChanged     = (_: com.serenity.ui.fonts.FontLoader.FontConfig) => IO.unit
    val deviceTextScaleProvider = IO.pure(1.0)
    val configPersistencePath   = None
    val uiPresetStore = UiPresetStore(Paths.get(System.getProperty("java.io.tmpdir"), "capability-ports-spec.json"))
    val windowSizeProvider = IO.pure(Option.empty[PreferredWindowSize])
    val bufferAnimationsRef =
      Ref.of[IO, Map[BufferId, com.serenity.animation.AnimationState]](Map.empty).unsafeRunSync()
    val markdownPreviewWindow = com.serenity.ui.tui.MarkdownPreviewWindowAvailability.Unavailable

  private def path(name: String): Path = Paths.get(name)

  "trackRecentFile" should "prepend a new path onto an empty list" in {
    port.trackRecentFile(Nil, path("a.txt")) shouldBe List(path("a.txt"))
  }

  it should "move an already-present path to the front rather than duplicate it" in {
    val current = List(path("a.txt"), path("b.txt"), path("c.txt"))

    port.trackRecentFile(current, path("c.txt")) shouldBe List(path("c.txt"), path("a.txt"), path("b.txt"))
  }

  it should "prepend a genuinely new path ahead of the existing ones" in {
    val current = List(path("a.txt"), path("b.txt"))

    port.trackRecentFile(current, path("z.txt")) shouldBe List(path("z.txt"), path("a.txt"), path("b.txt"))
  }

  it should "cap the result at 20 entries, dropping the oldest" in {
    val current = (1 to 20).map(i => path(s"file-$i.txt")).toList

    val updated = port.trackRecentFile(current, path("new.txt"))

    updated should have size 20
    updated.head shouldBe path("new.txt")
    updated should not contain path("file-20.txt")
  }
