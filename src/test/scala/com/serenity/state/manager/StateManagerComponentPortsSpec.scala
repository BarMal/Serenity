package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.keystroke.events.ResizeEvent
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, SurfaceId}
import com.serenity.state.reducers.{ReducerResult, WorkflowEffect}
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerComponentPortsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "ResizeEventHandler" should "apply the reduced transition before rebalancing through its narrow port" in {
    val program = for
      observed <- Ref.of[IO, List[String]](Nil)
      handler = new ResizeEventHandler(new ResizeEventPort:
        def applyReducerResult(result: ReducerResult, fallbackState: AppState): IO[Unit] =
          observed.update(_ :+ s"resize:${result.state.runtime.viewportSize}")
        def rebalancePanes(): IO[Unit] =
          observed.update(_ :+ "rebalance"))
      _     <- handler.apply(ResizeEvent(ViewportSize(90, 30)), AppState.initial)
      calls <- observed.get
    yield calls

    program.unsafeRunSync() shouldBe List("resize:Some(ViewportSize(90,30))", "rebalance")
  }

  it should "not rebalance when state application fails" in {
    val program = for
      rebalanced <- Ref.of[IO, Boolean](false)
      handler = new ResizeEventHandler(new ResizeEventPort:
        def applyReducerResult(result: ReducerResult, fallbackState: AppState): IO[Unit] =
          IO.raiseError(new IllegalStateException("state failure"))
        def rebalancePanes(): IO[Unit] = rebalanced.set(true))
      result <- handler.apply(ResizeEvent(ViewportSize(90, 30)), AppState.initial).attempt
      value  <- rebalanced.get
    yield (result, value)

    program.unsafeRunSync() match
      case (Left(error), false) => error.getMessage shouldBe "state failure"
      case other                => fail(s"Unexpected resize result: $other")
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
        def submitFile(surfaceId: SurfaceId): IO[Unit]    = calls.update(_ :+ s"file:$surfaceId")
        def submitReplace(surfaceId: SurfaceId): IO[Unit] = calls.update(_ :+ s"replace:$surfaceId")
        def submitClose(surfaceId: SurfaceId): IO[Unit]   = calls.update(_ :+ s"close:$surfaceId"))
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
