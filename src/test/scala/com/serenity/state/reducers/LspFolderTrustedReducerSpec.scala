package com.serenity.state.reducers

import java.nio.file.Path

import com.serenity.config.AppMode
import com.serenity.keystroke.events.LspEvent
import com.serenity.lsp.LspEffect
import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{Diagnostic, LspPosition, LspRange}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Trusting a folder lets its language servers start (#2526): the documents already open under it are announced again,
  * as entering code mode announces them, since the server was refused when they first opened.
  */
class LspFolderTrustedReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val root    = Path.of("/tmp/serenity-trust-spec/app")
  private val inside  = root.resolve("src/Main.scala")
  private val nested  = root.resolve("modules/core/Core.scala")
  private val outside = Path.of("/tmp/serenity-trust-spec/other/Other.scala")
  private val prefix  = Path.of("/tmp/serenity-trust-spec/app-sibling/Sibling.scala")

  private def buffer(id: Int, path: Path): (BufferId, Buffer) =
    BufferId(id) -> Buffer(
      BufferId(id),
      Document(Rope(s"// $id"), filePath = Some(path), language = Some(LanguageId.Scala))
    )

  private def withBuffers(mode: AppMode): AppState =
    val state = AppState.initial
    state.copy(persisted =
      state.persisted.copy(
        config = state.persisted.config.withAppMode(mode),
        buffers = Map(buffer(0, inside), buffer(1, outside), buffer(2, nested), buffer(3, prefix))
      )
    )

  private def reopened(result: ReducerResult): List[String] =
    result.effects.collect {
      case AppEffect.LspQueue(LspQueueEffect.Enqueue(LspEffect.FileOpened(uri, _, _))) =>
        uri
    }

  "A trusted folder" should "announce again every open document under it, and only those" in {
    val result = SystemEventReducer.reduce(LspEvent.LspFolderTrusted(root), withBuffers(AppMode.Code))

    reopened(result) shouldBe List(inside.toUri.toString, nested.toUri.toString)
  }

  it should "change no state of its own" in {
    val state = withBuffers(AppMode.Code)

    SystemEventReducer.reduce(LspEvent.LspFolderTrusted(root), state).state shouldBe state
  }

  it should "announce nothing in prose mode, which has no servers" in {
    reopened(SystemEventReducer.reduce(LspEvent.LspFolderTrusted(root), withBuffers(AppMode.Prose))) shouldBe empty
  }

  "Revoked trust" should "announce again the open documents under the revoked folders, and only those" in {
    val result = SystemEventReducer.reduce(LspEvent.LspTrustRevoked(List(root)), withBuffers(AppMode.Code))

    reopened(result) shouldBe List(inside.toUri.toString, nested.toUri.toString)
  }

  it should "drop the server data of those documents and keep every other document's" in {
    val diagnostic = Diagnostic(LspRange(LspPosition(0, 0), LspPosition(0, 1)), None, "stale")
    val held       = withBuffers(AppMode.Code)
    val withData = held.copy(runtime =
      held.runtime.copy(languageService =
        held.runtime.languageService.copy(diagnosticsState =
          DiagnosticsState(diagnostics =
            Map(
              DocumentUri(inside.toUri.toString)  -> List(diagnostic),
              DocumentUri(outside.toUri.toString) -> List(diagnostic)
            )
          )
        )
      )
    )

    val after = SystemEventReducer.reduce(LspEvent.LspTrustRevoked(List(root)), withData).state
    val left  = after.runtime.languageService.diagnosticsState.diagnostics

    left.get(DocumentUri(inside.toUri.toString)).forall(_.isEmpty) shouldBe true
    left.get(DocumentUri(outside.toUri.toString)) shouldBe Some(List(diagnostic))
  }

  it should "announce nothing in prose mode" in {
    reopened(SystemEventReducer.reduce(LspEvent.LspTrustRevoked(List(root)), withBuffers(AppMode.Prose))) shouldBe empty
  }
