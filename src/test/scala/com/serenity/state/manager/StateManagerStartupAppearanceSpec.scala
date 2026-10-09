package com.serenity.state.manager

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.TestTemp
import com.serenity.config.{AppConfig, ThemeFollowConfig}
import com.serenity.rope.Balance
import com.serenity.ui.theme.appearance.{OsAppearance, OsAppearanceDetector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory

/** Before the first frame the OS appearance is read within a bound: the frame is drawn in the theme the OS asks for,
  * and a detector that does not answer in time costs the bound and no more.
  */
class StateManagerStartupAppearanceSpec extends AnyFlatSpec with Matchers:

  given Balance                                  = Balance.default
  given org.typelevel.log4cats.LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def managerWith(followSystem: Boolean, detector: OsAppearanceDetector): IO[StateManager] =
    IO.blocking(TestTemp.directory("startup-appearance")).flatMap { root =>
      StateManager(
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO],
        sessionRootOverride = Some(root),
        initialConfig = AppConfig.default.withThemeFollowConfig(ThemeFollowConfig(followSystem = followSystem)),
        appearanceDetector = detector
      )
    }

  private def themeName(manager: StateManager): IO[String] = manager.getCurrentState.map(_.persisted.theme.name)

  private def detecting(delay: FiniteDuration, appearance: OsAppearance): OsAppearanceDetector =
    new OsAppearanceDetector:
      def detect: IO[OsAppearance] = IO.sleep(delay).as(appearance)

  private val silent: OsAppearanceDetector = new OsAppearanceDetector:
    def detect: IO[OsAppearance] = IO.never

  "Following the system appearance before the first frame" should "have the OS theme in place when it returns" in {
    val program =
      for
        manager <- managerWith(followSystem = true, OsAppearanceDetector.fixed(OsAppearance.Light))
        _       <- manager.followSystemAppearanceWithin(1.second)
        theme   <- themeName(manager)
      yield theme

    program.unsafeRunSync() shouldBe "light"
  }

  it should "wait for a detector that answers inside the bound" in {
    val program =
      for
        manager <- managerWith(followSystem = true, detecting(100.millis, OsAppearance.HighContrast))
        _       <- manager.followSystemAppearanceWithin(5.seconds)
        theme   <- themeName(manager)
      yield theme

    program.unsafeRunSync() shouldBe "high-contrast"
  }

  it should "fall back to the configured theme when the detector does not answer in time" in {
    val program =
      for
        manager <- managerWith(followSystem = true, silent)
        before  <- themeName(manager)
        started <- IO.monotonic
        _       <- manager.followSystemAppearanceWithin(150.millis).timeout(10.seconds)
        elapsed <- IO.monotonic.map(_ - started)
        after   <- themeName(manager)
      yield (before, after, elapsed)

    val (before, after, elapsed) = program.unsafeRunSync()
    after shouldBe before
    elapsed should be >= 150.millis
    elapsed should be < 5.seconds
  }

  it should "leave a detector that never answers able to be asked again later" in {
    val program =
      for
        answers <- Ref.of[IO, OsAppearance](OsAppearance.Unknown)
        slowThenFast = new OsAppearanceDetector:
          def detect: IO[OsAppearance] = answers.get.flatMap {
            case OsAppearance.Unknown => IO.never
            case known                => IO.pure(known)
          }
        manager <- managerWith(followSystem = true, slowThenFast)
        _       <- manager.followSystemAppearanceWithin(100.millis).timeout(10.seconds)
        _       <- answers.set(OsAppearance.Light)
        _       <- manager.followSystemAppearance
        theme   <- themeName(manager)
      yield theme

    program.unsafeRunSync() shouldBe "light"
  }

  it should "not query the OS at all when follow_system is off" in {
    val program =
      for
        reads <- Ref.of[IO, Int](0)
        counting = new OsAppearanceDetector:
          def detect: IO[OsAppearance] = reads.update(_ + 1).as(OsAppearance.Light)
        manager <- managerWith(followSystem = false, counting)
        before  <- themeName(manager)
        _       <- manager.followSystemAppearanceWithin(1.second)
        after   <- themeName(manager)
        count   <- reads.get
      yield (before, after, count)

    val (before, after, count) = program.unsafeRunSync()
    after shouldBe before
    count shouldBe 0
  }
