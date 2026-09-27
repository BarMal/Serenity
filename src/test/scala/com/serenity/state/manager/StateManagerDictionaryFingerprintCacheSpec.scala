package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.foldable.*
import com.serenity.config.{AppConfig, SpellCheckConfig, SpellCheckDictionaryFingerprint}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1691: `scheduleDocumentAnalysis` runs after every successful commit (`ModelCommit.commit`, via
  * `StateManagerOperationBoundary.afterCommit`) -- effectively every keystroke -- so a per-commit filesystem stat to
  * discover the spell-check dictionaries' fingerprints is a real per-keystroke cost. These specs exercise
  * `StateManagerOperationBoundary`'s cache directly, via the `discoverDictionaryFingerprints` seam
  * `StateManagerOperationBoundary.create` takes for exactly this purpose, standing in for the real
  * `SpellCheckConfig.discoverDictionaryFingerprints`/`Files.*` calls. `refreshDictionaryFingerprints` itself is
  * invalidation-signal-agnostic -- `AppRuntimeExternalChangeWatchSpec` covers the primary, real-time
  * `FileChangeWatcher`-driven trigger; this spec calls the same method directly to cover the cache mechanics and the
  * window focus-gain backstop that also calls it.
  */
class StateManagerDictionaryFingerprintCacheSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private def spellCheckEnabledState(content: String = "hello"): AppState =
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config =
          AppConfig.default.withSpellCheck(AppConfig.default.languageToolsConfig.spellCheck.copy(enabled = true)),
        buffers =
          val buffer = AppState.initial.persisted.buffers(bufferId)
          AppState.initial.persisted.buffers
            .updated(bufferId, buffer.copy(document = buffer.document.copy(content = com.serenity.rope.Rope(content))))
      )
    )

  private def countingDiscovery(
    calls: Ref[IO, Int],
    fingerprints: IO[List[SpellCheckDictionaryFingerprint]]
  ): SpellCheckConfig => IO[List[SpellCheckDictionaryFingerprint]] =
    _ => calls.update(_ + 1) >> fingerprints

  private def fixedFingerprint(tag: Long): List[SpellCheckDictionaryFingerprint] =
    List(
      SpellCheckDictionaryFingerprint(
        path = "/dictionaries/en.dic",
        exists = true,
        isDirectory = false,
        size = tag,
        lastModifiedMillis = tag
      )
    )

  "scheduleDocumentAnalysis" should
    "not re-discover dictionary fingerprints across repeated validated commits with an unchanged spell-check config" in {
      val state = spellCheckEnabledState()
      val program = for
        modelRef   <- ModelViews.modelOf(state)
        calls      <- Ref.of[IO, Int](0)
        operations <- StateManagerOperationBoundary.create(
          modelRef,
          org.typelevel.log4cats.noop.NoOpLogger.impl[IO],
          discoverDictionaryFingerprints = countingDiscovery(calls, IO.pure(fixedFingerprint(1L)))
        )
        // Warms the cache -- this first commit is expected to pay for exactly one discovery.
        _                <- operations.modelCommit.commitState(state, state)
        callsAfterWarmup <- calls.get
        // 100 further validated commits against the same (unchanged) spell-check config.
        _               <- (1 to 100).toList.traverse_(_ => operations.modelCommit.commitState(state, state))
        callsAfterEdits <- calls.get
        _               <- operations.shutdownEffects()
      yield
        callsAfterWarmup shouldBe 1
        callsAfterEdits shouldBe 1 // zero *additional* stat calls across the 100 edits

      program.unsafeRunSync()
    }

  it should "re-discover dictionary fingerprints when the spell-check config actually changes" in {
    val initialState = spellCheckEnabledState()
    val reconfiguredState = initialState.copy(
      persisted = initialState.persisted.copy(
        config = initialState.persisted.config.withSpellCheck(
          initialState.persisted.config.languageToolsConfig.spellCheck.copy(additionalWords = List("wurld"))
        )
      )
    )
    val program = for
      modelRef   <- ModelViews.modelOf(initialState)
      calls      <- Ref.of[IO, Int](0)
      operations <- StateManagerOperationBoundary.create(
        modelRef,
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO],
        discoverDictionaryFingerprints = countingDiscovery(calls, IO.pure(fixedFingerprint(1L)))
      )
      _                <- operations.modelCommit.commitState(initialState, initialState)
      callsAfterFirst  <- calls.get
      _                <- operations.modelCommit.commitState(reconfiguredState, initialState)
      callsAfterConfig <- calls.get
      _                <- operations.shutdownEffects()
    yield
      callsAfterFirst shouldBe 1
      callsAfterConfig shouldBe 2

    program.unsafeRunSync()
  }

  "refreshDictionaryFingerprints" should
    "make an on-disk dictionary change picked up on the next commit, still without a stat on every commit" in {
      val state = spellCheckEnabledState()
      val program = for
        modelRef           <- ModelViews.modelOf(state)
        calls              <- Ref.of[IO, Int](0)
        currentFingerprint <- Ref.of[IO, List[SpellCheckDictionaryFingerprint]](fixedFingerprint(1L))
        analysisStarts     <- Ref.of[IO, Int](0)
        operations <- StateManagerOperationBoundary.create(
          modelRef,
          org.typelevel.log4cats.noop.NoOpLogger.impl[IO],
          beforeDocumentAnalysisStart = analysisStarts.update(_ + 1),
          discoverDictionaryFingerprints = countingDiscovery(calls, currentFingerprint.get)
        )
        // Warms the cache and starts the first analysis pass for the buffer's initial content.
        _                 <- operations.modelCommit.commitState(state, state)
        startsAfterWarmup <- analysisStarts.get
        callsAfterWarmup  <- calls.get
        // A repeated commit against the same content and the same on-disk dictionary triggers nothing further.
        _                 <- operations.modelCommit.commitState(state, state)
        startsAfterRepeat <- analysisStarts.get
        callsAfterRepeat  <- calls.get
        // The dictionary file changes on disk -- nothing in the (unchanged) spell-check config reflects that.
        _ <- currentFingerprint.set(fixedFingerprint(2L))
        // Without an explicit refresh, the stale cached fingerprint is still served and nothing re-triggers.
        _                 <- operations.modelCommit.commitState(state, state)
        startsBeforeFocus <- analysisStarts.get
        // Window focus-gain busts the cache (#1691's chosen invalidation signal) with exactly one fresh discovery.
        _               <- operations.refreshDictionaryFingerprints()
        callsAfterFocus <- calls.get
        // The next commit now sees a different dictionary fingerprint for the same buffer content, and re-analyzes.
        _                <- operations.modelCommit.commitState(state, state)
        startsAfterFocus <- analysisStarts.get
        _                <- operations.shutdownEffects()
      yield
        startsAfterWarmup shouldBe 1
        callsAfterWarmup shouldBe 1
        startsAfterRepeat shouldBe 1 // no re-analysis from a plain repeated commit
        callsAfterRepeat shouldBe 1 // ...and no additional stat call either
        startsBeforeFocus shouldBe 1 // the stale cache masks the on-disk change until refreshed
        callsAfterFocus shouldBe 2 // refreshDictionaryFingerprints is the one deliberate extra stat
        startsAfterFocus shouldBe 2 // and now re-analysis fires, proving the change is still detected (issue #1691's
        // second acceptance criterion)

      program.unsafeRunSync()
    }

  "dictionaryWatchDirectories" should
    "reflect the current spell-check config's configured dictionary directory" in {
      val configuredDirectory = java.nio.file.Files.createTempDirectory("serenity-watch-dictionary-boundary")
      val state = AppState.initial.copy(
        persisted = AppState.initial.persisted.copy(
          config = AppConfig.default.withSpellCheck(
            AppConfig.default.languageToolsConfig.spellCheck
              .copy(enabled = true, dictionaryPaths = List(configuredDirectory.toString))
          )
        )
      )
      val program = for
        modelRef    <- ModelViews.modelOf(state)
        operations  <- StateManagerOperationBoundary.create(modelRef, org.typelevel.log4cats.noop.NoOpLogger.impl[IO])
        directories <- operations.dictionaryWatchDirectories
        _           <- operations.shutdownEffects()
      yield directories.should(contain(configuredDirectory))

      program.unsafeRunSync()
    }

  it should "be empty once the spell-check config disabling it has committed" in {
    val disabledState = AppState.initial
    val program = for
      modelRef    <- ModelViews.modelOf(disabledState)
      operations  <- StateManagerOperationBoundary.create(modelRef, org.typelevel.log4cats.noop.NoOpLogger.impl[IO])
      directories <- operations.dictionaryWatchDirectories
      _           <- operations.shutdownEffects()
    yield directories.shouldBe(Set.empty)

    program.unsafeRunSync()
  }
