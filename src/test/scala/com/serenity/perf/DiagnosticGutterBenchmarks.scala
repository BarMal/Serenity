package com.serenity.perf

import cats.effect.unsafe.IORuntime
import com.serenity.config.AppConfig
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import com.serenity.rope.Balance
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.updateState
import com.serenity.state.models.{BufferId, DiagnosticsState, LanguageServiceState}

/** `laptop.render.typing_frame_2x` over a buffer with an error on every line, so the gutter holds a `!` marker beside
  * every visible row. The keystroke edits one row; the markers of the rest are kept, so a frame that redraws them all
  * pays for rows it did not touch. Once with a cell-grid counter (code) and once with a measured one (prose).
  */
private[perf] object DiagnosticGutterBenchmarks:

  given Balance = Balance.default

  private val TypingLine = 150

  private def withErrorOnEveryLine(stateManager: StateManager, bufferId: BufferId, lines: Int)(using
    IORuntime
  ): StateManager =
    stateManager
      .updateState { state =>
        val buffer = state.persisted.buffers(bufferId)
        val diagnostics = (0 until lines).toList.map(line =>
          Diagnostic(
            range = LspRange(LspPosition(line, 0), LspPosition(line, 1)),
            severity = Some(DiagnosticSeverity.Error),
            message = "problem",
            source = Some("benchmark"),
            code = Some("benchmark")
          )
        )
        state.copy(runtime =
          state.runtime.copy(languageService =
            LanguageServiceState(diagnosticsState =
              DiagnosticsState(diagnostics = Map(SpellChecker.diagnosticsUri(buffer) -> diagnostics))
            )
          )
        )
      }
      .unsafeRunSync()
    stateManager

  private def asScala(stateManager: StateManager, bufferId: BufferId)(using IORuntime): StateManager =
    stateManager
      .updateState { state =>
        val buffer = state.persisted.buffers(bufferId)
        state.copy(persisted =
          state.persisted.copy(buffers =
            state.persisted.buffers
              .updated(bufferId, buffer.copy(document = buffer.document.copy(language = Some(LanguageId.Scala))))
          )
        )
      }
      .unsafeRunSync()
    stateManager

  def benchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
    val (prose, proseId) =
      LaptopFrameBenchmarks.proseStateManager(config = AppConfig.conventional.withLineNumbers(true))
    val (code, codeId) =
      LaptopFrameBenchmarks.stateManagerHolding(
        BenchmarkFixtures.loremIpsumProse(300),
        AppConfig.conventional.withLineNumbers(true),
        cursorLine = TypingLine,
        cursorColumn = 4
      )
    List(
      EreaderTypingBenchmarks.typingBenchmark(
        "laptop.render.typing_frame_2x.diagnostics_gutter_measured",
        withErrorOnEveryLine(prose, proseId, lines = 300)
      ),
      EreaderTypingBenchmarks.typingBenchmark(
        "laptop.render.typing_frame_2x.diagnostics_gutter_cells",
        withErrorOnEveryLine(asScala(code, codeId), codeId, lines = 300)
      )
    )
