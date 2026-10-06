package com.serenity.state.manager

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.rope.{Balance, Rope}
import com.serenity.spellcheck.DictionaryCache
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Document analysis loads its dictionary through the cache it was given, so managers that share one cache on purpose
  * (the benchmark harness builds many in one process) parse the dictionary once between them; a manager given none
  * keeps its own (#1677).
  */
class StateManagerDictionaryCacheSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private val prose: AppState =
    val initial = AppState.initial
    initial.copy(persisted =
      initial.persisted.copy(
        config = AppConfig.default.withSpellCheck(SpellCheckConfig(enabled = true)),
        buffers = initial.persisted.buffers.updated(
          bufferId,
          initial.persisted
            .buffers(bufferId)
            .copy(document = initial.persisted.buffers(bufferId).document.copy(content = Rope("hello world")))
        )
      )
    )

  private def loadedWithin(cache: DictionaryCache): IO[Boolean] =
    (1 to 200).foldLeft(IO.pure(false)) { (found, _) =>
      found.flatMap(already => if already then IO.pure(true) else IO.sleep(50.millis).as(cache.size > 0))
    }

  private def analyse(cache: DictionaryCache): IO[Boolean] =
    for
      modelRef   <- ModelViews.modelOf(prose)
      operations <- StateManagerOperationBoundary.create(modelRef, NoOpLogger.impl[IO], dictionaryCache = cache)
      _          <- operations.modelCommit.commitState(prose.copy(), prose)
      loaded     <- loadedWithin(cache)
      _          <- operations.shutdownEffects()
    yield loaded

  "A state manager's document analysis" should "load the dictionary through the cache it was given" in {
    val shared = DictionaryCache()

    analyse(shared).unsafeRunSync() shouldBe true
    shared.size shouldBe 1
  }
