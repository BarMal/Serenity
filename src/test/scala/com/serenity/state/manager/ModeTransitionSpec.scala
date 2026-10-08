package com.serenity.state.manager

import com.serenity.config.AppMode
import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspProgressTask, LspRange}
import com.serenity.project.{ProjectTaskCommand, ProjectTaskKind}
import com.serenity.rope.Balance
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a change of app mode settles in the state itself, inside the commit that makes it, so no frame ever shows
  * language-server data or a project task in a workspace that has none.
  */
class ModeTransitionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val uri = DocumentUri("file:///project/A.scala")

  private def range = LspRange(LspPosition(0, 0), LspPosition(0, 3))

  private val serverComplaint = Diagnostic(range, Some(DiagnosticSeverity.Error), "type mismatch", Some("metals"))

  private val misspelling =
    Diagnostic(range, Some(DiagnosticSeverity.Warning), "Unknown word", Some(SpellChecker.Source))

  private val build =
    ProjectTaskCommand(ProjectTaskKind.Build, "sbt", java.nio.file.Paths.get("/project"), "sbt", List("compile"))

  private def inMode(mode: AppMode)(state: AppState): AppState =
    state.copy(persisted = state.persisted.copy(config = state.persisted.config.withAppMode(mode)))

  private val codeWithServerData: AppState =
    val initial = inMode(AppMode.Code)(AppState.initial)
    initial.copy(runtime =
      initial.runtime.copy(
        languageService = LanguageServiceState(
          diagnosticsState = DiagnosticsState(diagnostics = Map(uri -> List(serverComplaint, misspelling))),
          semanticTokensState = SemanticTokensState(unavailableUris = Set(uri)),
          progress = Map(LanguageId.Scala -> List(LspProgressTask("t", "Indexing", None, None)))
        ),
        projectTasks = ProjectTasks(nextId = 4, running = Some(RunningProjectTask(3, build, "compiling")))
      )
    )

  private def model(state: AppState): Model = Model(state, UndoState())

  private def settledOnLeavingCode: AppState =
    val prose = inMode(AppMode.Prose)(codeWithServerData)
    ModeTransition.settled(codeWithServerData, model(prose)).app

  "A model whose app mode did not change" should "come back as the very same instance" in {
    val next = model(codeWithServerData.copy(runtime = codeWithServerData.runtime.copy(clipboard = Some("x"))))

    ModeTransition.settled(codeWithServerData, next) should be theSameInstanceAs next
  }

  "Leaving code mode" should "drop what the language servers reported, and the work they were doing" in {
    val languageService = settledOnLeavingCode.runtime.languageService

    languageService.diagnosticsState.diagnostics.values.flatten.map(_.source) should not contain Some("metals")
    languageService.semanticTokensState shouldBe SemanticTokensState()
    languageService.progress shouldBe Map.empty
  }

  it should "keep the spell-check marks, which belong to prose" in {
    settledOnLeavingCode.runtime.languageService.diagnosticsState.diagnostics shouldBe Map(uri -> List(misspelling))
  }

  it should "release the running project task, keeping the counter that versions task results" in {
    val tasks = settledOnLeavingCode.runtime.projectTasks

    tasks.running shouldBe None
    tasks.nextId shouldBe 4
    tasks.terminalText should startWith("Project task stopped")
  }

  "Entering code mode" should "leave the state as it is" in {
    val prose = inMode(AppMode.Prose)(codeWithServerData)
    val next  = model(inMode(AppMode.Code)(prose))

    ModeTransition.settled(prose, next) should be theSameInstanceAs next
  }
