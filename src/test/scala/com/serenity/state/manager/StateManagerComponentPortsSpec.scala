package com.serenity.state.manager

import cats.effect.{Fiber, IO, Ref}
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import com.serenity.command.Command
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.ResizeEvent
import com.serenity.lsp.LspEffect
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.AppEffect
import com.serenity.state.undo.UndoState
import com.serenity.ui.layout.{PanelPosition, ViewportSize}
import com.serenity.ui.presets.UiPresetStore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerComponentPortsSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  "StateManagerEventPipelineBehavior" should "route a resize through only its declared port" in {
    val program = for
      state        <- Ref.of[IO, AppState](AppState.initial)
      undo         <- Ref.of[IO, UndoState](UndoState())
      queue        <- Queue.bounded[IO, LspEffect](4)
      analysis     <- Ref.of[IO, Option[Fiber[IO, Throwable, Unit]]](None)
      mouseTargets <- Ref.of[IO, Option[MouseTargetCache]](None)
      port = new StateManagerEventPipelineDependencies:
        val stateRef                 = state
        val undoRef                  = undo
        val lspQueue                 = queue
        val documentAnalysisFiberRef = analysis
        val mouseTargetCacheRef      = mouseTargets
        val uiPresetStore            = UiPresetStore.default
        val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerComponentPortsSpec.event"))
        def beginCloseAction(scope: CloseScope, current: AppState): IO[Unit]                  = IO.unit
        def interpretEffect(effect: AppEffect): IO[Unit]                                      = IO.unit
        def interpretCommand(command: Command, current: AppState): IO[Unit]                   = IO.unit
        def createBuffer(content: String, filePath: Option[java.nio.file.Path]): IO[BufferId] = IO.pure(BufferId(0))
        def createPane(bufferId: Option[BufferId]): IO[PaneId]                                = IO.pure(PaneId(0))
        def updateConfig(update: AppConfig => AppConfig): IO[AppConfig] =
          state.modify(s =>
            val config = update(s.config)
            s.copy(config = config) -> config
          )
        def executeCommand(command: Command): IO[Unit]                         = IO.unit
        def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit] = IO.unit
      behavior = new StateManagerEventPipelineBehavior(port)
      _       <- behavior.applyEvent(ResizeEvent(ViewportSize(90, 30)))
      current <- state.get
    yield current.config shouldBe AppConfig.default

    program.unsafeRunSync()
  }
