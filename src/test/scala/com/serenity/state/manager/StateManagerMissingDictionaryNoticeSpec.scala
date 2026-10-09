package com.serenity.state.manager

import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.PeekStateReducer
import com.serenity.ui.layout.PeekContent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** #1680: spell check with no dictionary to check against says so once, rather than silently doing nothing. */
class StateManagerMissingDictionaryNoticeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private def stateWith(spellCheck: SpellCheckConfig, content: String): AppState =
    val initial = AppState.initial
    initial.copy(persisted =
      initial.persisted.copy(
        config = AppConfig.default.withSpellCheck(spellCheck),
        buffers = initial.persisted.buffers.updated(
          bufferId,
          initial.persisted
            .buffers(bufferId)
            .copy(document = initial.persisted.buffers(bufferId).document.withContent(Rope(content)))
        )
      )
    )

  private def withLaterContent(state: AppState, content: String): AppState =
    val buffer = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(bufferId, buffer.copy(document = buffer.document.withContent(Rope(content))))
      )
    )

  private def noticeText(state: AppState): Option[String] =
    state.peekSurface.map(_.content).collect { case SurfaceContent.QuickInfo(text) => text }

  private def eventually[A](read: IO[Option[A]]): IO[Option[A]] =
    (1 to 100).foldLeft(IO.pure(Option.empty[A])) { (found, _) =>
      found.flatMap(already => if already.isDefined then IO.pure(already) else IO.sleep(50.millis) >> read)
    }

  "Document analysis with no dictionary to check against" should "show one notice naming where it looked" in {
    val emptyDirectory = Files.createTempDirectory("serenity-no-dictionary")
    val spellCheck =
      SpellCheckConfig(enabled = true, languages = List("en-US"), dictionaryPaths = List(emptyDirectory.toString))
    val first  = stateWith(spellCheck, "hello")
    val second = withLaterContent(first, "hello again")

    val program = for
      modelRef   <- ModelViews.modelOf(first)
      operations <- StateManagerOperationBoundary.create(modelRef, NoOpLogger.impl[IO])
      _          <- operations.modelCommit.commitState(first.copy(), first)
      shown      <- eventually(ModelViews.appRef(modelRef).get.map(noticeText))
      // `second` carries no notice of its own, so one showing after its analysis would be a second announcement.
      _     <- operations.modelCommit.commitState(second, first)
      _     <- IO.sleep(1.second)
      later <- ModelViews.appRef(modelRef).get.map(noticeText)
      _     <- operations.shutdownEffects()
    yield (shown, later)

    val (shown, later) = program.unsafeRunSync()

    shown.getOrElse(fail("expected a notice")) should (include("en-US") and include(emptyDirectory.toString))
    later shouldBe None
  }

  it should "show no notice when spell check is off" in {
    val emptyDirectory = Files.createTempDirectory("serenity-no-dictionary")
    val state = stateWith(SpellCheckConfig(enabled = false, dictionaryPaths = List(emptyDirectory.toString)), "hello")

    val program = for
      modelRef   <- ModelViews.modelOf(state)
      operations <- StateManagerOperationBoundary.create(modelRef, NoOpLogger.impl[IO])
      _          <- operations.modelCommit.commitState(state.copy(), state)
      _          <- IO.sleep(1.second)
      notice     <- ModelViews.appRef(modelRef).get.map(noticeText)
      _          <- operations.shutdownEffects()
    yield notice

    program.unsafeRunSync() shouldBe None
  }

  it should "show no notice while there is nothing to check" in {
    val emptyDirectory = Files.createTempDirectory("serenity-no-dictionary")
    val state = stateWith(
      SpellCheckConfig(enabled = true, languages = List("en-US"), dictionaryPaths = List(emptyDirectory.toString)),
      ""
    )

    val program = for
      modelRef   <- ModelViews.modelOf(state)
      operations <- StateManagerOperationBoundary.create(modelRef, NoOpLogger.impl[IO])
      _          <- operations.modelCommit.commitState(state.copy(), state)
      _          <- IO.sleep(1.second)
      notice     <- ModelViews.appRef(modelRef).get.map(noticeText)
      _          <- operations.shutdownEffects()
    yield notice

    program.unsafeRunSync() shouldBe None
  }

  it should "leave a peek that is already showing alone, and announce once that is gone" in {
    val emptyDirectory = Files.createTempDirectory("serenity-no-dictionary")
    val spellCheck =
      SpellCheckConfig(enabled = true, languages = List("en-US"), dictionaryPaths = List(emptyDirectory.toString))
    val plain = stateWith(spellCheck, "hello")
    val withPeek =
      PeekStateReducer.show(PeekContent.QuickInfo("something the writer just did"), CursorPosition(0, 0), plain).state
    val later = withLaterContent(plain, "hello again")

    val program = for
      modelRef   <- ModelViews.modelOf(withPeek)
      operations <- StateManagerOperationBoundary.create(modelRef, NoOpLogger.impl[IO])
      _          <- operations.modelCommit.commitState(withPeek.copy(), withPeek)
      _          <- IO.sleep(1.second)
      during     <- ModelViews.appRef(modelRef).get.map(noticeText)
      _          <- operations.modelCommit.commitState(later, withPeek)
      after      <- eventually(ModelViews.appRef(modelRef).get.map(noticeText))
      _          <- operations.shutdownEffects()
    yield (during, after)

    val (during, after) = program.unsafeRunSync()

    during shouldBe Some("something the writer just did")
    after.getOrElse(fail("expected the notice once the peek was gone")) should include("en-US")
  }
