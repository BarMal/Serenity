package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.TestTemp
import com.serenity.config.{AppConfig, ConfigError, ConfigLoadResult}
import com.serenity.keystroke.events.Event
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader
import org.typelevel.log4cats.noop.NoOpLogger

/** A [[StateManagerConfigEffects]] over a real config path in a temp directory and an [[EffectEditorPort]] double. */
trait ConfigEffectsHarness:

  given Balance = Balance.default

  final protected class Harness(
      val stateRef: Ref[IO, AppState],
      val fontConfigs: Ref[IO, List[FontLoader.FontConfig]],
      val analysisRuns: Ref[IO, Int],
      val events: Ref[IO, List[Event]],
      val committedStates: Ref[IO, List[AppState]],
      val notices: Ref[IO, List[Notice]],
      val configPath: Option[Path],
      val config: StateManagerConfigEffects
  ):
    def currentConfig: AppConfig = stateRef.get.unsafeRunSync().persisted.config

  protected def harness(
    initialState: AppState = AppState.initial,
    persistConfig: Boolean = true,
    deviceTextScale: Double = 1.0,
    configOnDisk: Option[AppConfig] = None,
    loadConfig: Path => IO[Either[ConfigError, ConfigLoadResult]] = ConfigFileSync.defaultLoad
  ): Harness =
    val root       = TestTemp.directory("config-effects-spec")
    val configPath = Option.when(persistConfig)(root.resolve("config.json"))
    val modelRef   = Ref.of[IO, Model](Model(initialState, UndoState())).unsafeRunSync()
    val stateRef   = ModelViews.appRef(modelRef)
    val fonts      = Ref.of[IO, List[FontLoader.FontConfig]](Nil).unsafeRunSync()
    val analyses   = Ref.of[IO, Int](0).unsafeRunSync()
    val events     = Ref.of[IO, List[Event]](Nil).unsafeRunSync()
    val committed  = Ref.of[IO, List[AppState]](Nil).unsafeRunSync()
    val notices    = Ref.of[IO, List[Notice]](Nil).unsafeRunSync()

    val editor = new EffectEditorPort:
      def enqueueEvent(event: Event): IO[Unit] = events.update(_ :+ event)
      def commitState(newState: AppState, fallbackState: AppState): IO[Unit] =
        committed.update(_ :+ newState) >> stateRef.set(newState)
      def updateModelValidated(transition: Model => Option[Model]): IO[Unit] =
        modelRef.get.flatMap(model =>
          transition(model).fold(IO.unit)(next => committed.update(_ :+ next.app) >> modelRef.set(next))
        )
      def scheduleDocumentAnalysis(): IO[Unit]                                               = analyses.update(_ + 1)
      def scheduleFindSearch(request: FindSearchRequest): IO[Unit]                           = IO.unit
      def submitEffect(lane: com.serenity.state.effects.Lane.Keyed, job: IO[Unit]): IO[Unit] = job
      def dispatchEffectResult(result: EffectResult, onApplied: AppState => IO[Unit]): IO[Unit] =
        stateRef.get.flatMap { current =>
          val next = EffectResult.applyIfCurrent(current, result)
          if next eq current then IO.unit else stateRef.set(next) >> onApplied(next)
        }

    new Harness(
      stateRef,
      fonts,
      analyses,
      events,
      committed,
      notices,
      configPath,
      new StateManagerConfigEffects(
        stateRef.get,
        NoOpLogger.impl[IO],
        configPath,
        fontConfig => fonts.update(_ :+ fontConfig),
        IO.pure(deviceTextScale),
        editor,
        com.serenity.state.manager.RenderCaches.create(),
        showNotice = notice => notices.update(_ :+ notice),
        configOnDisk = configOnDisk,
        loadConfig = loadConfig
      )
    )

  protected def editExternally(fixture: Harness, text: String): Unit =
    fixture.configPath.foreach(path => Files.writeString(path, text): Unit)

  protected def reload(fixture: Harness): Unit =
    fixture.config.watch.foreach(_.reload.unsafeRunSync())
