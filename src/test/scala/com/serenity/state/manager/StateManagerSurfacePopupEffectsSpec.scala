package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.command.ThemeIntent
import com.serenity.io.FileDialog
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, SurfaceEffect, ThemeEffect}
import com.serenity.testkit.AwaitCondition.awaitValue
import com.serenity.testkit.VirtualTime.runVirtual
import com.serenity.ui.theme.config.{AppThemeManager, ThemeConfig, ThemeConfigWriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Exercises [[StateManagerSurfacePopupEffects]] on its own: the theme picker/creator, theme switching/reload, theme
  * export, and the file-search overlay, each asserted through the state it lands (or the collaborator it calls) rather
  * than through a fully composed `StateManager`.
  */
class StateManagerSurfacePopupEffectsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private class Harness(
      val stateRef: Ref[IO, AppState],
      val committed: Ref[IO, List[AppState]],
      val themeNamesRef: Ref[IO, List[String]],
      val popups: StateManagerSurfacePopupEffects
  )

  private def harness(
    initialState: AppState = AppState.initial,
    themeNames: List[String] = Nil,
    fileDialog: Option[FileDialog] = None
  ): Harness =
    val stateRef      = Ref.of[IO, AppState](initialState).unsafeRunSync()
    val committed     = Ref.of[IO, List[AppState]](Nil).unsafeRunSync()
    val themeNamesRef = Ref.of[IO, List[String]](themeNames).unsafeRunSync()
    new Harness(
      stateRef,
      committed,
      themeNamesRef,
      popupsOver(
        stateRef,
        committed,
        themeNamesRef,
        EffectLanePortFixtures.immediate(stateRef),
        AppThemeManager.create,
        fileDialog
      )
    )

  private def popupsOver(
    stateRef: Ref[IO, AppState],
    committed: Ref[IO, List[AppState]],
    themeNamesRef: Ref[IO, List[String]],
    lanes: EffectLanePort,
    themeManager: AppThemeManager,
    fileDialog: Option[FileDialog] = None
  ): StateManagerSurfacePopupEffects =
    def commitState(newState: AppState, fallbackState: AppState): IO[Unit] =
      committed.update(_ :+ newState) >> stateRef.set(newState)

    lazy val popups: StateManagerSurfacePopupEffects = new StateManagerSurfacePopupEffects(
      stateRef.get,
      NoOpLogger.impl[IO],
      themeManager,
      themeNamesRef,
      fileDialog,
      commitState,
      lanes,
      {
        case AppEffect.Theme(effect)                           => popups.interpretThemeEffect(effect)
        case AppEffect.Surface(SurfaceEffect.OpenThemePicker)  => stateRef.get.flatMap(popups.openThemePickerEffect)
        case AppEffect.Surface(SurfaceEffect.OpenThemeCreator) => stateRef.get.flatMap(popups.openThemeCreatorEffect)
        case _                                                 => IO.unit
      }
    )
    popups

  "StateManagerSurfacePopupEffects" should "toggle from the initial dark theme to light" in {
    val fixture = harness()
    AppState.initial.persisted.theme.name shouldBe "dark"

    fixture.popups.interpretThemeIntent(ThemeIntent.ToggleTheme, AppState.initial).unsafeRunSync()

    fixture.stateRef.get.unsafeRunSync().persisted.theme.name shouldBe "light"
  }

  it should "toggle back from light to dark" in {
    val fixture = harness()
    fixture.popups.interpretThemeIntent(ThemeIntent.ToggleTheme, AppState.initial).unsafeRunSync()
    val lightState = fixture.stateRef.get.unsafeRunSync()

    fixture.popups.interpretThemeIntent(ThemeIntent.ToggleTheme, lightState).unsafeRunSync()

    fixture.stateRef.get.unsafeRunSync().persisted.theme.name shouldBe "dark"
  }

  it should "reload the buffer's current theme by name" in {
    val fixture = harness()

    fixture.popups.interpretThemeIntent(ThemeIntent.ReloadTheme, AppState.initial).unsafeRunSync()

    fixture.stateRef.get.unsafeRunSync().persisted.theme.name shouldBe AppState.initial.persisted.theme.name
  }

  it should "open the theme picker seeded with the current theme's index when theme names are loaded" in {
    val fixture = harness(themeNames = List("default-light", "default-dark", "dracula"))

    fixture.popups.interpretThemeIntent(ThemeIntent.OpenThemeChooser, AppState.initial).unsafeRunSync()

    val after = fixture.stateRef.get.unsafeRunSync()
    after.runtime.uiSurfaces.map(_.content) match
      case List(SurfaceContent.ThemePicker(pickerState)) =>
        pickerState.themes shouldBe List("default-light", "default-dark", "dracula")
        pickerState.originalTheme shouldBe AppState.initial.persisted.theme.name
      case other => fail(s"Expected a single ThemePicker surface, got $other")
  }

  it should "do nothing when opening the theme picker before any theme names have loaded" in {
    val fixture = harness(themeNames = Nil)

    fixture.popups.interpretThemeIntent(ThemeIntent.OpenThemeChooser, AppState.initial).unsafeRunSync()

    fixture.committed.get.unsafeRunSync() shouldBe Nil
  }

  it should "open the theme creator seeded from the current theme" in {
    val fixture = harness()

    fixture.popups.interpretThemeIntent(ThemeIntent.OpenThemeCreator, AppState.initial).unsafeRunSync()

    fixture.stateRef.get.unsafeRunSync().runtime.uiSurfaces.map(_.content) match
      case List(SurfaceContent.ThemeCreator(_)) => succeed
      case other                                => fail(s"Expected a single ThemeCreator surface, got $other")
  }

  it should "replace rather than duplicate an already-open theme creator surface" in {
    val fixture = harness()
    fixture.popups.interpretThemeIntent(ThemeIntent.OpenThemeCreator, AppState.initial).unsafeRunSync()
    val withCreator = fixture.stateRef.get.unsafeRunSync()

    fixture.popups.interpretThemeIntent(ThemeIntent.OpenThemeCreator, withCreator).unsafeRunSync()

    fixture.stateRef.get.unsafeRunSync().runtime.uiSurfaces.count {
      _.content match
        case SurfaceContent.ThemeCreator(_) => true
        case _                              => false
    } shouldBe 1
  }

  it should "open the file-search overlay and focus it" in {
    val fixture = harness()

    fixture.popups.interpretThemeIntent(ThemeIntent.ReloadThemes, AppState.initial).unsafeRunSync()
    fixture.popups.openFileSearchEffect(AppState.initial).unsafeRunSync()

    val after = fixture.stateRef.get.unsafeRunSync()
    after.runtime.uiSurfaces.map(_.content) match
      case List(SurfaceContent.FileSearch(searchState)) =>
        searchState.query shouldBe ""
        after.persisted.focus shouldBe Focus.Surface(after.runtime.uiSurfaces.head.id)
      case other => fail(s"Expected a single FileSearch surface, got $other")
  }

  it should "load and store the available theme names on ReloadThemes" in {
    val fixture = harness()

    fixture.popups.interpretThemeIntent(ThemeIntent.ReloadThemes, AppState.initial).unsafeRunSync()

    fixture.themeNamesRef.get.unsafeRunSync() should not be empty
  }

  it should "do nothing exporting the current theme when no file dialog is available" in {
    val fixture = harness(fileDialog = None)

    fixture.popups.interpretThemeIntent(ThemeIntent.ExportCurrentTheme, AppState.initial).unsafeRunSync()

    fixture.committed.get.unsafeRunSync() shouldBe Nil
  }

  it should "export the current theme to the path chosen through the file dialog" in {
    val directory = Files.createTempDirectory("theme-export")
    try
      val dialog = FileDialog(
        chooseOpenFile = _ => IO.pure(None),
        chooseSaveFile = (_, suggestedFileName) => IO.pure(suggestedFileName.map(directory.resolve))
      )

      val fixture = harness(fileDialog = Some(dialog))

      fixture.popups.interpretThemeIntent(ThemeIntent.ExportCurrentTheme, AppState.initial).unsafeRunSync()

      Files.list(directory).count() shouldBe 1
    finally
      Files.list(directory).forEach(Files.deleteIfExists)
      Files.deleteIfExists(directory)
  }

  it should "refresh the theme names only once a saved theme has been written" in {
    val savedNames = List("dark", "light", "my-theme")
    val config     = ThemeConfigWriter.themeToConfig(AppState.initial.persisted.theme).copy(name = "my-theme")

    val program =
      for
        stateRef      <- Ref.of[IO, AppState](AppState.initial)
        committed     <- Ref.of[IO, List[AppState]](Nil)
        themeNamesRef <- Ref.of[IO, List[String]](List("dark", "light"))
        written       <- Deferred[IO, Unit]
        gatedWrites = new AppThemeManager:
          override def listAvailableThemes: IO[List[String]]         = IO.pure(savedNames)
          override def writeUserTheme(config: ThemeConfig): IO[Path] = written.get.as(Path.of("my-theme.conf"))
        result <- EffectLanePortFixtures.laned(stateRef).use { lanes =>
          val popups = popupsOver(
            stateRef,
            committed,
            themeNamesRef,
            lanes,
            gatedWrites
          )
          for
            _           <- popups.interpretThemeEffect(ThemeEffect.SaveThemeConfig(config))
            _           <- IO.sleep(1.second)
            beforeWrite <- stateRef.get.map(_.runtime.availableThemeNames)
            _           <- written.complete(())
            afterWrite  <- awaitValue(stateRef.get.map(_.runtime.availableThemeNames))(_ == savedNames)
            pickerNames <- themeNamesRef.get
          yield (beforeWrite, afterWrite, pickerNames)
        }
      yield result

    runVirtual(program) shouldBe (Nil, savedNames, savedNames)
  }
