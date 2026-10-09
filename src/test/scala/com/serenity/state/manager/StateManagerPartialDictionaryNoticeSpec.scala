package com.serenity.state.manager

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** A language with no dictionary is announced once per session even when other configured languages resolved. */
class StateManagerPartialDictionaryNoticeSpec extends AnyFlatSpec with Matchers:

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
            .copy(document = initial.persisted.buffers(bufferId).document.copy(content = Rope(content)))
        )
      )
    )

  private def noticeText(state: AppState): Option[String] =
    state.peekSurface.map(_.content).collect { case SurfaceContent.QuickInfo(text) => text }

  private def eventually[A](read: IO[Option[A]]): IO[Option[A]] =
    (1 to 100).foldLeft(IO.pure(Option.empty[A])) { (found, _) =>
      found.flatMap(already => if already.isDefined then IO.pure(already) else IO.sleep(50.millis) >> read)
    }

  "Document analysis with one of two languages lacking a dictionary" should "show one notice naming only that language" in {
    val directory = TestTemp.directory("serenity-partial-notice")
    Files.writeString(directory.resolve("en_US.dic"), "1\ncolour", StandardCharsets.UTF_8)
    Files.writeString(directory.resolve("en_US.aff"), "SET UTF-8", StandardCharsets.UTF_8)
    val spellCheck =
      SpellCheckConfig(enabled = true, languages = List("en-US", "fr"), dictionaryPaths = List(directory.toString))
    val first  = stateWith(spellCheck, "colour")
    val second = stateWith(spellCheck, "colour again")

    val program = for
      modelRef   <- ModelViews.modelOf(first)
      operations <- StateManagerOperationBoundary.create(modelRef, NoOpLogger.impl[IO])
      _          <- operations.modelCommit.commitState(first.copy(), first)
      shown      <- eventually(ModelViews.appRef(modelRef).get.map(noticeText))
      _          <- operations.modelCommit.commitState(second, first)
      _          <- IO.sleep(1.second)
      later      <- ModelViews.appRef(modelRef).get.map(noticeText)
      _          <- operations.shutdownEffects()
    yield (shown, later)

    val (shown, later) = program.unsafeRunSync()

    val text = shown.getOrElse(fail("expected a notice for fr"))
    text should (include("hunspell-fr") and not include "hunspell-en-us")
    later shouldBe None
  }
