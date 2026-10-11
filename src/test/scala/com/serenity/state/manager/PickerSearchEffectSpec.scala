package com.serenity.state.manager

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.serenity.io.ProjectFileListing
import com.serenity.keystroke.events.ModalInsertChar
import com.serenity.state.effects.Lane
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, WorkflowEffect}
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Rapid typing in a big picker leaves only the latest query's match applied: each keystroke's match supersedes the one
  * still running on the PickerSearch lane (#1926).
  */
class PickerSearchEffectSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val root = Path.of("/work/project")

  private val opened: (AppState, SurfaceId) =
    val files = (0 until ListPickerSearch.CheapFileCount + 1)
      .map(index => Path.of(s"src/module$index/File$index.scala"))
      .toVector
    val (state, id) = FileFinderTransitions.withFinderOpened(AppState.initial, root).getOrElse(fail("no finder"))
    FileFinderTransitions.withFilesListed(state, id, root, Right(ProjectFileListing(files, false))) -> id

  private def typing(state: AppState, char: Char): AppState =
    ModalEventReducer.reduce(ModalType.ListPicker, ModalInsertChar(char), state).state

  private def labels(state: AppState, id: SurfaceId): Vector[String] =
    state.runtime.uiSurfaces
      .find(_.id == id)
      .collect { case UiSurface(_, SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)), _, _) => picker }
      .flatMap(_.items.toOption)
      .fold(Vector.empty)(_.items.map(_.label))

  private def recording(inner: EffectLanePort, landed: Ref[IO, List[String]]): EffectLanePort =
    new EffectLanePort:
      def submitEffect(lane: Lane.Keyed, job: IO[Unit]): IO[Unit] =
        inner.submitEffect(lane, job)
      def dispatchEffectResult(result: EffectResult, onApplied: AppState => IO[Unit]): IO[Unit] =
        val note = result match
          case EffectResult.PickerSearched(_, query, _) => landed.update(_ :+ query)
          case _                                        => IO.unit
        note >> inner.dispatchEffectResult(result, onApplied)

  "A keystroke in a large finder" should "ask for its match on the PickerSearch lane" in {
    val (state, id) = opened

    ModalEventReducer.reduce(ModalType.ListPicker, ModalInsertChar('1'), state).effects shouldBe
      List(AppEffect.Workflow(WorkflowEffect.RefreshPicker(id, "1")))
  }

  "Matches requested while typing" should "apply only the latest query's, the ones still running abandoned" in {
    val (initial, id) = opened
    val afterOne      = typing(initial, '1')
    val afterTwelve   = typing(afterOne, '2')
    val afterLatest   = typing(afterTwelve, '3')

    val program =
      for
        stateRef <- Ref.of[IO, AppState](afterOne)
        landed   <- Ref.of[IO, List[String]](Nil)
        gate     <- Deferred[IO, Unit]
        search = (state: AppState, surface: SurfaceId, query: String) =>
          gate.get.whenA(query != "123") >> IO.delay(PickerSearchTransitions.searched(state, surface, query))
        _ <- EffectLanePortFixtures.laned(stateRef).use { laned =>
          val lanes = recording(laned, landed)
          for
            _ <- PickerSearchEffect.refresh(lanes, stateRef.get, id, "1", search)
            _ <- IO.sleep(10.millis)
            _ <- stateRef.set(afterTwelve)
            _ <- PickerSearchEffect.refresh(lanes, stateRef.get, id, "12", search)
            _ <- IO.sleep(10.millis)
            _ <- stateRef.set(afterLatest)
            _ <- PickerSearchEffect.refresh(lanes, stateRef.get, id, "123", search)
            _ <- IO.sleep(10.millis)
            _ <- gate.complete(())
            _ <- IO.sleep(1.second)
          yield ()
        }
        applied <- landed.get
        shown   <- stateRef.get
      yield (applied, labels(shown, id))

    val (applied, shown) = runVirtual(program)

    applied shouldBe List("123")
    val expected = PickerSearchTransitions.applied(afterLatest, id, "123", searchedFor(afterLatest, id)).state
    shown shouldBe labels(expected, id)
    shown should not be labels(afterLatest, id)
  }

  private def searchedFor(state: AppState, id: SurfaceId): ListPicker =
    PickerSearchTransitions.searched(state, id, "123").getOrElse(fail("no result"))
