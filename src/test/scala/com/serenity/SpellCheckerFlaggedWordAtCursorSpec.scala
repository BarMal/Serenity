package com.serenity

import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.rope.{Balance, Rope}
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Regression cover for #1531: the "add word to dictionary" action resolves the flagged word directly from the
  * buffer's current content at the diagnostic's range, rather than reparsing the diagnostic message. Split out of
  * `SpellCheckerSpec` to keep that file under the architecture ratchet's file-length target.
  */
class SpellCheckerFlaggedWordAtCursorSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  it should "find the word covered by the spell-check diagnostic at the cursor" in {
    val config     = SpellCheckConfig(enabled = true)
    val bufferId   = BufferId(0)
    val text       = "hello wurld today"
    val baseBuffer = AppState.initial.persisted.buffers(bufferId)
    val buffer = baseBuffer.copy(
      document = baseBuffer.document.copy(content = Rope(text)),
      editing = EditingState(List(CursorPosition(0, 8)))
    )
    val uri         = SpellChecker.diagnosticsUri(buffer)
    val diagnostics = SpellChecker.check(text, config)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withSpellCheck(config),
        buffers = Map(bufferId -> buffer)
      ),
      runtime = AppState.initial.runtime.copy(
        diagnosticsState = AppState.initial.runtime.diagnosticsState.copy(diagnostics = Map(uri -> diagnostics))
      )
    )

    SpellChecker.flaggedWordAtCursor(state) shouldBe Some("wurld")
  }

  it should "not resolve a flagged word when the cursor sits outside every diagnostic range" in {
    val config     = SpellCheckConfig(enabled = true)
    val bufferId   = BufferId(0)
    val text       = "hello wurld today"
    val baseBuffer = AppState.initial.persisted.buffers(bufferId)
    val buffer = baseBuffer.copy(
      document = baseBuffer.document.copy(content = Rope(text)),
      editing = EditingState(List(CursorPosition(0, 2)))
    )
    val uri         = SpellChecker.diagnosticsUri(buffer)
    val diagnostics = SpellChecker.check(text, config)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withSpellCheck(config),
        buffers = Map(bufferId -> buffer)
      ),
      runtime = AppState.initial.runtime.copy(
        diagnosticsState = AppState.initial.runtime.diagnosticsState.copy(diagnostics = Map(uri -> diagnostics))
      )
    )

    SpellChecker.flaggedWordAtCursor(state) shouldBe None
  }

  it should "not resolve a flagged word when there is no active buffer" in {
    val state = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map.empty))

    SpellChecker.flaggedWordAtCursor(state) shouldBe None
  }
