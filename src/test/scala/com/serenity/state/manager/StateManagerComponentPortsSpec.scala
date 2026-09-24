package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.keystroke.events.ResizeEvent
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, CloseScope, PaneId, SurfaceId}
import com.serenity.state.reducers.{ReducerResult, WorkflowEffect}
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerComponentPortsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "ResizeEventHandler" should "apply the resize and its pane rebalance as one reduced transition through its narrow port" in {
    val focusedWithoutActivePane =
      AppState.initial.copy(persisted =
        AppState.initial.persisted.copy(layout = AppState.initial.persisted.layout.copy(activeEditorPaneId = None))
      )
    val program = for
      observed <- Ref.of[IO, List[AppState]](Nil)
      handler = new ResizeEventHandler(
        new ResizeEventPort:
          def applyReducerResult(result: ReducerResult, fallbackState: AppState): IO[Unit] =
            observed.update(_ :+ result.state)
      )
      _     <- handler.apply(ResizeEvent(ViewportSize(90, 30)), focusedWithoutActivePane)
      calls <- observed.get
    yield calls

    program.unsafeRunSync() match
      case List(applied) =>
        applied.runtime.viewportSize shouldBe Some(ViewportSize(90, 30))
        applied.persisted.layout.activeEditorPaneId shouldBe Some(PaneId(0))
      case other => fail(s"Expected exactly one applied transition, got $other")
  }

  it should "propagate a failure to apply the transition" in {
    val handler = new ResizeEventHandler(
      new ResizeEventPort:
        def applyReducerResult(result: ReducerResult, fallbackState: AppState): IO[Unit] =
          IO.raiseError(new IllegalStateException("state failure"))
    )

    handler.apply(ResizeEvent(ViewportSize(90, 30)), AppState.initial).attempt.unsafeRunSync() match
      case Left(error) => error.getMessage shouldBe "state failure"
      case other       => fail(s"Unexpected resize result: $other")
  }

  "WorkflowEffectHandler" should "route only its declared workflow operation and propagate failures" in {
    val program = for
      calls <- Ref.of[IO, List[String]](Nil)
      handler = new WorkflowEffectHandler(new WorkflowEffectPort:
        def requestOpenFile: IO[Unit]               = calls.update(_ :+ "open")
        def requestSaveAs: IO[Unit]                 = IO.raiseError(new IllegalStateException("save-as failed"))
        def refresh(surfaceId: SurfaceId): IO[Unit] = calls.update(_ :+ s"refresh:$surfaceId")
        def refreshFind(request: com.serenity.state.models.FindSearchRequest): IO[Unit] =
          calls.update(_ :+ s"find:${request.query}")
        def submitFile(surfaceId: SurfaceId): IO[Unit]        = calls.update(_ :+ s"file:$surfaceId")
        def openAsProjectRoot(surfaceId: SurfaceId): IO[Unit] = calls.update(_ :+ s"open-as-root:$surfaceId")
        def submitReplace(surfaceId: SurfaceId): IO[Unit]     = calls.update(_ :+ s"replace:$surfaceId")
        def beginClose(scope: CloseScope): IO[Unit]           = calls.update(_ :+ s"begin-close:$scope")
        def submitClose(surfaceId: SurfaceId): IO[Unit]       = calls.update(_ :+ s"close:$surfaceId")
        def submitReloadConflict(surfaceId: SurfaceId): IO[Unit] =
          calls.update(_ :+ s"reload-conflict:$surfaceId")
        def createDirectories(surfaceId: SurfaceId): IO[Unit] = calls.update(_ :+ s"create-dirs:$surfaceId")
        def submitSessionNamePrompt(surfaceId: SurfaceId): IO[Unit] =
          calls.update(_ :+ s"session-name-prompt:$surfaceId")
        def submitSessionList(surfaceId: SurfaceId): IO[Unit] = calls.update(_ :+ s"session-list:$surfaceId"))
      _       <- handler.interpret(WorkflowEffect.RequestOpenFile)
      failure <- handler.interpret(WorkflowEffect.RequestSaveAs).attempt
      seen    <- calls.get
    yield (failure, seen)

    program.unsafeRunSync() match
      case (Left(error), List("open")) => error.getMessage shouldBe "save-as failed"
      case other                       => fail(s"Unexpected workflow result: $other")
  }

  "LifecycleEffectHandler" should "complete quit through its only declared operation" in {
    val completed = Ref.of[IO, Int](0).unsafeRunSync()
    val handler = new LifecycleEffectHandler(
      new LifecycleEffectPort:
        def completeQuit: IO[Unit] = completed.update(_ + 1)
    )

    handler.interpret.unsafeRunSync()

    completed.get.unsafeRunSync() shouldBe 1
  }
