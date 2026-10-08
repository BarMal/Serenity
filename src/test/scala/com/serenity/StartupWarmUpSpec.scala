package com.serenity

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import com.serenity.app.StartupWarmUp
import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendRuntime
import com.serenity.keystroke.events.*
import com.serenity.lsp.model.SemanticTokenData
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{AppState, AppStateValidation, Damage, Focus}
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

class StartupWarmUpSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = NoOpFactory[IO]

  private val viewport  = ViewportSize(100, 30)
  private val smallPlan = StartupWarmUp.Plan(rounds = 2, paragraphs = 12)

  final private case class Drawn(full: Vector[(AppState, Damage)], cursorOnly: Int)

  private def recordingFrames(drawn: Ref[IO, Drawn], afterFull: IO[Unit] = IO.unit) =
    Resource.pure[IO, FrontendRuntime.OffscreenFrames](
      FrontendRuntime.OffscreenFrames(
        full = (state, damage, _) => drawn.update(d => d.copy(full = d.full :+ (state -> damage))) >> afterFull,
        cursorOnly = (_, _) => drawn.update(d => d.copy(cursorOnly = d.cursorOnly + 1))
      )
    )

  private def warmUpDirectories(scratchRoot: Path): Set[Path] =
    Files.list(scratchRoot).iterator().asScala.filter(_.getFileName.toString.startsWith("serenity-warm-up")).toSet

  "StartupWarmUp.document" should "be deterministic prose, one paragraph per line with blank lines between" in {
    val document = StartupWarmUp.document(paragraphs = 5)
    document shouldBe StartupWarmUp.document(paragraphs = 5)
    document.split("\n\n", -1).length shouldBe 5
    document.split("\n\n").foreach(paragraph => paragraph.split(' ').length should be >= 40)
  }

  "StartupWarmUp.seeded" should "put the document in the focused pane of a state that still validates" in {
    val content = StartupWarmUp.document(paragraphs = 8)
    val seeded = StartupWarmUp.seeded(AppState.initial, content).getOrElse(fail("the initial state has an editor pane"))
    val paneId = seeded.persisted.layout.activeEditorPaneId.getOrElse(fail("no active pane"))
    val buffer = seeded.persisted.layout.editorPanes
      .get(paneId)
      .flatMap(_.bufferId)
      .flatMap(seeded.persisted.buffers.get)
      .getOrElse(fail("no buffer in the active pane"))

    buffer.document.content.toString shouldBe content
    buffer.document.isDirty shouldBe false
    seeded.persisted.focus shouldBe Focus.EditorPane(paneId)
    AppStateValidation.validationErrors(seeded) shouldBe Nil
  }

  "StartupWarmUp.interruptsWarmUp" should "treat keys and clicks as real input, but not hovering or system events" in {
    StartupWarmUp.interruptsWarmUp(InsertChar('a')) shouldBe true
    StartupWarmUp.interruptsWarmUp(MoveDown) shouldBe true
    StartupWarmUp.interruptsWarmUp(MouseClick(1, 1)) shouldBe true
    StartupWarmUp.interruptsWarmUp(ScrollDown(3)) shouldBe true
    StartupWarmUp.interruptsWarmUp(MouseMove(1, 1)) shouldBe false
    StartupWarmUp.interruptsWarmUp(ResizeEvent(viewport)) shouldBe false
    StartupWarmUp.interruptsWarmUp(LspEvent.LspSemanticTokensReceived("file:///a", SemanticTokenData.empty)) shouldBe
      false
  }

  "StartupWarmUp.run" should "type, delete and move through a throwaway editor, drawing every step off-screen" in {
    val program = for
      drawn   <- Ref.of[IO, Drawn](Drawn(Vector.empty, 0))
      noInput <- Deferred[IO, Unit]
      outcome <- StartupWarmUp.run(AppConfig.default, Theme.dark, viewport, recordingFrames(drawn), noInput, smallPlan)
      result  <- drawn.get
    yield (outcome, result)

    val (outcome, drawn) = program.unsafeRunTimed(60.seconds).getOrElse(fail("the warm-up did not finish"))
    val steps            = smallPlan.rounds * StartupWarmUp.round.size

    outcome shouldBe StartupWarmUp.Outcome.Completed(steps)
    drawn.full.size shouldBe steps
    drawn.cursorOnly shouldBe steps
    val lengths = drawn.full.map((state, _) => focusedLength(state)).distinct
    withClue("the edits never reached the document: ")(lengths.size should be > 1)
    drawn.full.map(_._2).distinct.size should be > 1
    drawn.full.foreach((state, _) => state.persisted.theme shouldBe Theme.dark)
  }

  it should "leave the session it starts from untouched, and clean up after itself" in {
    val sessionRoot = Files.createTempDirectory("serenity-warm-up-spec-session")
    val scratchRoot = Files.createTempDirectory("serenity-warm-up-spec-scratch")
    val program = for
      user <- StateManager.apply(
        NoOpLogger[IO],
        sessionRootOverride = Some(sessionRoot),
        dictionaryCache = SharedDictionary.default
      )
      _ <- user.applyEvent(ResizeEvent(viewport))
      _ <- "draft".toList.traverse_(char => user.applyEvent(InsertChar(char)))
      _ <- user.runtimeLifecycle.awaitEffects
      before      = warmUpDirectories(scratchRoot)
      filesBefore = sessionFiles(sessionRoot)
      modelBefore <- user.getModel
      drawn       <- Ref.of[IO, Drawn](Drawn(Vector.empty, 0))
      noInput     <- Deferred[IO, Unit]
      _ <- StartupWarmUp.run(
        modelBefore.app.persisted.config,
        modelBefore.app.persisted.theme,
        viewport,
        recordingFrames(drawn),
        noInput,
        smallPlan,
        scratchRoot
      )
      modelAfter <- user.getModel
    yield (modelBefore, modelAfter, filesBefore, sessionFiles(sessionRoot), before, warmUpDirectories(scratchRoot))

    val (modelBefore, modelAfter, filesBefore, filesAfter, directoriesBefore, directoriesAfter) =
      program.unsafeRunTimed(60.seconds).getOrElse(fail("the warm-up did not finish"))

    modelAfter shouldBe modelBefore
    filesAfter shouldBe filesBefore
    directoriesAfter shouldBe directoriesBefore
  }

  it should "keep its session directory inside the scratch root it is given, removing it when done" in {
    val scratchRoot = Files.createTempDirectory("serenity-warm-up-spec-scratch")
    val program = for
      seen    <- Ref.of[IO, Set[Path]](Set.empty)
      drawn   <- Ref.of[IO, Drawn](Drawn(Vector.empty, 0))
      noInput <- Deferred[IO, Unit]
      observe = IO.blocking(warmUpDirectories(scratchRoot)).flatMap(found => seen.update(_ ++ found))
      _ <- StartupWarmUp.run(
        AppConfig.default,
        Theme.dark,
        viewport,
        recordingFrames(drawn, afterFull = observe),
        noInput,
        smallPlan,
        scratchRoot
      )
      during <- seen.get
    yield during

    val during = program.unsafeRunTimed(60.seconds).getOrElse(fail("the warm-up did not finish"))

    during.size shouldBe 1
    warmUpDirectories(scratchRoot) shouldBe Set.empty
  }

  // The input lands from inside the third frame, so the loop itself must see it before a fourth step starts: waiting on
  // the race to cancel lets a busy scheduler run extra steps against the user's first keystrokes.
  it should "stop at the first real input instead of finishing its plan" in {
    val program = for
      drawn      <- Ref.of[IO, Drawn](Drawn(Vector.empty, 0))
      firstInput <- Deferred[IO, Unit]
      thirdFrame = drawn.get.flatMap(d => IO.whenA(d.full.size == 3)(firstInput.complete(()).void))
      outcome <- StartupWarmUp.run(
        AppConfig.default,
        Theme.dark,
        viewport,
        recordingFrames(drawn, afterFull = thirdFrame),
        firstInput,
        StartupWarmUp.Plan(rounds = 50, paragraphs = 12)
      )
      atReturn <- drawn.get
      _        <- IO.sleep(300.millis)
      later    <- drawn.get
    yield (outcome, atReturn.full.size, later.full.size)

    val (outcome, framesAtReturn, framesLater) =
      program.unsafeRunTimed(60.seconds).getOrElse(fail("the warm-up did not stop"))

    outcome shouldBe StartupWarmUp.Outcome.Interrupted
    framesAtReturn shouldBe 3
    framesLater shouldBe framesAtReturn
  }

  private def focusedLength(state: AppState): Int =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(state.persisted.buffers.get)
      .map(_.document.content.weight)
      .getOrElse(-1)

  private def sessionFiles(root: Path): Map[Path, Long] =
    Files.walk(root).iterator().asScala.map(path => path -> Files.getLastModifiedTime(path).toMillis).toMap
