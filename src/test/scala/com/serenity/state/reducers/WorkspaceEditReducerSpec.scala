package com.serenity.state.reducers

import java.nio.file.Path

import com.serenity.keystroke.events.LspEvent
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{LspPosition, LspProgress, LspRange, LspTextEdit}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a server's `workspace/applyEdit` and `$/progress` do to application state (#1847). */
class WorkspaceEditReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val firstId    = BufferId(0)
  private val secondId   = BufferId(1)
  private val firstPath  = Path.of("/tmp/serenity-apply-edit-spec/First.scala")
  private val secondPath = Path.of("/tmp/serenity-apply-edit-spec/Second.scala")

  private def buffer(id: BufferId, path: Path, text: String): Buffer =
    Buffer(id, Document(Rope(text), filePath = Some(path), language = Some(LanguageId.Scala)))

  private val twoBuffers: AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers =
        Map(firstId -> buffer(firstId, firstPath, "val a = 1"), secondId -> buffer(secondId, secondPath, "val b = 2"))
      )
    )

  private def replace(from: Int, to: Int, text: String): LspTextEdit =
    LspTextEdit(LspRange(LspPosition(0, from), LspPosition(0, to)), text)

  private def textOf(state: AppState, id: BufferId): String =
    state.persisted.buffers(id).document.content.collect()

  "A workspace edit" should "change every open buffer it names, focused or not" in {
    val edits = Map(
      firstPath.toUri.toString  -> List(replace(4, 5, "first")),
      secondPath.toUri.toString -> List(replace(4, 5, "second"))
    )

    val result = SystemEventReducer.reduce(LspEvent.LspWorkspaceEditRequested(edits), twoBuffers)

    textOf(result.state, firstId) shouldBe "val first = 1"
    textOf(result.state, secondId) shouldBe "val second = 2"
  }

  it should "leave buffers it does not name untouched" in {
    val edits  = Map(secondPath.toUri.toString -> List(replace(4, 5, "second")))
    val result = SystemEventReducer.reduce(LspEvent.LspWorkspaceEditRequested(edits), twoBuffers)

    textOf(result.state, firstId) shouldBe "val a = 1"
    textOf(result.state, secondId) shouldBe "val second = 2"
  }

  "Server progress" should "track a task from begin through report to end" in {
    def progress(state: AppState, update: LspProgress): AppState =
      SystemEventReducer.reduce(LspEvent.LspProgressReceived(LanguageId.Scala, "index", update), state).state
    def tasks(state: AppState) = state.runtime.languageService.progress.get(LanguageId.Scala).toList.flatten

    val begun    = progress(twoBuffers, LspProgress.Begin("Indexing", None, Some(0)))
    val reported = progress(begun, LspProgress.Report(Some("src/Main.scala"), Some(40)))
    val ended    = progress(reported, LspProgress.End(None))

    tasks(begun).map(_.display) shouldBe List("Indexing 0%")
    tasks(reported).map(_.display) shouldBe List("Indexing src/Main.scala 40%")
    tasks(ended) shouldBe Nil
  }

  it should "ignore a report for a task that never began" in {
    val result = SystemEventReducer.reduce(
      LspEvent.LspProgressReceived(LanguageId.Scala, "ghost", LspProgress.Report(None, Some(5))),
      twoBuffers
    )

    result.state.runtime.languageService.progress shouldBe empty
  }

  it should "be forgotten when its server stops" in {
    val running = SystemEventReducer
      .reduce(
        LspEvent.LspProgressReceived(LanguageId.Scala, "index", LspProgress.Begin("Indexing", None, None)),
        twoBuffers
      )
      .state

    SystemEventReducer
      .reduce(LspEvent.LspServerStopped(LanguageId.Scala), running)
      .state
      .runtime
      .languageService
      .progress shouldBe empty
  }
