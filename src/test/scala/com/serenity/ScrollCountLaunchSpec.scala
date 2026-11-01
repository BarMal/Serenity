package com.serenity

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import com.serenity.app.AppRuntime
import com.serenity.config.*
import com.serenity.frontend.{Frontend, FrontendRuntime, GuiFrontend, TuiFrontend}
import com.serenity.input.InputHandler
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{KeyStrokeInfo, KeyboardFidelityTier}
import com.serenity.rope.Balance
import com.serenity.state.manager.{RenderCaches, StateManager}
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.*
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** A pane's rows are counted late only if the launch that makes the app's state manager says so (#2445): the first
  * version of the change handed late counting to the state manager `AppRuntime` makes for itself, and the one `Main`
  * makes was left counting inside the frame, so the window never saw a late bar. These specs run `AppRuntime.run`
  * itself: the counting it hands to whoever makes the state manager is late for a graphical frontend, and when a count
  * lands the render loop is told to repaint that pane's scrollbar.
  */
class ScrollCountLaunchSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val viewport = ViewportSize(100, 30)
  private val paneId   = PaneId(0)

  private class SilentInputHandler extends InputHandler[IO]:
    override def keyStrokeInfoStream: Stream[IO, KeyStrokeInfo] = Stream.never
    override def eventStream: Stream[IO, Event]                 = Stream.never
    override def shutdown: IO[Unit]                             = IO.unit

  /** A document longer than the screen, in a state of its own: the scene it makes needs its rows counted. */
  private def longDocument: AppState =
    val buffer =
      Buffer.fromString(BufferId(1), Vector.tabulate(300)(line => s"line $line " + "lorem ipsum " * 20).mkString("\n"))
    val initial = AppState.initial(AppConfig.conventional)
    val state = initial.copy(
      runtime = initial.runtime.copy(viewportSize = Some(viewport)),
      persisted = initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), paneId)))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )
    LayoutEngine.syncViewportDimensions(state, viewport)

  final private case class Seen(firstSceneHadBar: Boolean, scrollbarDamage: Set[PaneId])

  /** Runs the app until its render loop is told a scrollbar needs repainting, or a few seconds pass. */
  private def runWith(frontend: Frontend): (Seen, Boolean) =
    given org.typelevel.log4cats.Logger[IO] = LoggerFactory[IO].getLogger(using LoggerName("ScrollCountLaunchSpec"))
    val outcome = (for
      repainted <- Deferred[IO, Set[PaneId]]
      firstBar  <- Deferred[IO, Boolean]
      render = (state: AppState, _: Boolean, _: Option[RenderColor], damage: Damage, caches: RenderCaches) =>
        for
          started <- firstBar.tryGet
          _ <-
            if started.isEmpty then
              IO(caches.authoritativeScene.forState(longDocument, viewport).paneScrolls.contains(paneId))
                .flatMap(firstBar.complete(_).void)
            else IO.unit
          panes = Damage.scrollbarPaneIds(damage)
          _ <- if panes.nonEmpty then repainted.complete(panes).void else IO.unit
        yield ()
      runtime = FrontendRuntime(
        inputHandler = _ => IO.pure(new SilentInputHandler),
        renderFull = render,
        renderCursorOnly = (_: AppState, _: Boolean, _: Option[RenderColor], _: Damage, _: RenderCaches) => IO.unit
      )
      done <- AppRuntime
        .run(
          initialViewportSize = viewport,
          checkResize = IO.pure(None),
          runtime = runtime,
          appConfig = AppConfig.default,
          makeStateManager = Some((logger, scrollCounting) =>
            StateManager.apply(
              logger,
              initialConfig = AppConfig.conventional,
              dictionaryCache = SharedDictionary.default,
              scrollCounting = scrollCounting
            )
          ),
          awaitExternalQuit = repainted.get.timeout(5.seconds).attempt.void,
          registerResizeCallback = _ => (),
          frontend = frontend
        )
        .timeout(15.seconds)
        .attempt
      panes <- repainted.tryGet
      bar   <- firstBar.tryGet
    yield (Seen(bar.getOrElse(true), panes.getOrElse(Set.empty)), done.isRight)).unsafeRunSync()
    outcome

  "AppRuntime.run with a graphical frontend" should "hand its state manager late counting and repaint the bar when a count lands" in {
    val (seen, finished) = runWith(GuiFrontend)

    finished shouldBe true
    seen.firstSceneHadBar shouldBe false
    seen.scrollbarDamage should contain(paneId)
  }

  "AppRuntime.run with a terminal frontend" should "keep counting inside the frame, so its first scene has the bar" in {
    val (seen, _) = runWith(TuiFrontend(KeyboardFidelityTier.Full))

    seen.firstSceneHadBar shouldBe true
    seen.scrollbarDamage shouldBe empty
  }
