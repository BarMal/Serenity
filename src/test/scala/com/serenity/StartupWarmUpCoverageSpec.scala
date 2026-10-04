package com.serenity

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref, Resource}
import com.serenity.app.StartupWarmUp
import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendRuntime
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, Buffer}
import com.serenity.ui.layout.{ViewportSize, WrappedLineCache}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The warm-up only pays off if it runs the paths a typist's keystroke takes, and those are the cache-missing ones: a
  * paragraph whose text just changed is measured whole, wrapped and indexed again. These specs pin that the warm-up
  * keeps producing such misses round after round, on wrapped prose, rather than replaying text the caches already hold.
  */
class StartupWarmUpCoverageSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(120, 40)

  final private case class Frame(state: AppState, wrappedParagraphs: Int)

  private def warmUp(plan: StartupWarmUp.Plan, config: AppConfig = AppConfig.default): Vector[Frame] =
    val program = for
      frames  <- Ref.of[IO, Vector[Frame]](Vector.empty)
      noInput <- Deferred[IO, Unit]
      _       <- StartupWarmUp.run(config, Theme.dark, viewport, recordingFrames(frames), noInput, plan)
      drawn   <- frames.get
    yield drawn
    program.unsafeRunTimed(120.seconds).getOrElse(fail("the warm-up did not finish"))

  private def recordingFrames(frames: Ref[IO, Vector[Frame]]): Resource[IO, FrontendRuntime.OffscreenFrames] =
    Resource.pure(
      FrontendRuntime.OffscreenFrames(
        full = (state, _, caches) =>
          caches.wrappedLines match
            case bounded: WrappedLineCache.Bounded => frames.update(_ :+ Frame(state, bounded.size))
            case other => IO.raiseError(new IllegalStateException(s"unbounded cache $other")),
        cursorOnly = (_, _) => IO.unit
      )
    )

  private def focusedBuffer(state: AppState): Option[Buffer] =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(state.persisted.buffers.get)

  private def paragraphLengths(content: String): List[Int] = content.split('\n').toList.map(_.length).filter(_ > 0)

  "The warm-up document" should "mix short paragraphs with ones of a thousand characters or more" in {
    val lengths = paragraphLengths(StartupWarmUp.document(StartupWarmUp.Plan.default.paragraphs))

    lengths.max should be >= 1_000
    lengths.min should be < 150
    lengths.distinct.size should be > lengths.size / 2
  }

  "StartupWarmUp.round" should "type fresh letters every round, taking back what it typed" in {
    val typed = (0 until 4).map(index => StartupWarmUp.round(index).collect { case InsertChar(char) => char })

    typed.distinct.size shouldBe typed.size
    (0 until 4).foreach { index =>
      val events = StartupWarmUp.round(index)
      events.count(_.isInstanceOf[InsertChar]) should be <= events.count(_ == DeleteBackward)
    }
  }

  it should "cover what a writer does between words: line ends, line breaks, vertical and paging moves" in {
    val events = (0 until 4).flatMap(StartupWarmUp.round).toSet

    events should contain allOf (
      NewLine,
      DeleteBackward,
      MoveToEnd,
      MoveToStart,
      MoveUp,
      MoveDown,
      MoveWordLeft,
      MoveWordRight,
      MoveLeft,
      MoveRight,
      PageDown,
      PageUp
    )
  }

  it should "be the same every time for the same index" in {
    StartupWarmUp.round(3) shouldBe StartupWarmUp.round(3)
  }

  "The warm-up" should "measure text the wrap cache has not seen in every round, not replay the same text" in {
    val cachedAfter = (1 to 4).map(rounds => warmUp(StartupWarmUp.Plan(rounds, paragraphs = 12)).last.wrappedParagraphs)

    cachedAfter.sliding(2).foreach {
      case Seq(fewer, more) =>
        withClue(s"cached paragraph wraps after 1 to 4 rounds: $cachedAfter; ")(more should be > fewer)
    }
  }

  it should "edit wrapped prose: word wrap on and a paragraph that spans many rows" in {
    val frames   = warmUp(StartupWarmUp.Plan(rounds = 2, paragraphs = 12))
    val wrapRows = viewport.width

    frames.head.state.persisted.config.surfaceConfig.wordWrapEnabled shouldBe true
    val longest = frames
      .flatMap(frame => focusedBuffer(frame.state))
      .map(buffer => paragraphLengths(buffer.document.content.toString).max)
    longest.max should be >= 8 * wrapRows
  }

  it should "move the caret across several prose paragraphs, not only blank or empty lines" in {
    val frames = warmUp(StartupWarmUp.Plan(rounds = 2, paragraphs = 12))
    val cursorLines = frames.flatMap { frame =>
      for
        buffer <- focusedBuffer(frame.state)
        cursor <- buffer.editing.cursorPositions.headOption
        line = buffer.document.content.toString.split("\n", -1).lift(cursor.line).getOrElse("")
      yield cursor.line -> line.nonEmpty
    }

    cursorLines.map(_._1).distinct.size should be >= 3
    cursorLines.count(_._2).toDouble / cursorLines.size should be > 0.6
  }
