package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.ui.layout.{Location, PanelPosition, Symbol, SymbolKind}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A docked outline catches up with edits through the real commit path, once the edit burst pauses. */
class PanelContentRefreshSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private val outlineId = SurfaceId("outline")

  private def withMarkdown(state: AppState, text: String): AppState =
    val bufferId = BufferId(0)
    val buffer   = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers + (bufferId -> buffer.copy(document =
          buffer.document.withContent(Rope(text)).copy(language = Some(LanguageId.Markdown))
        ))
      )
    )

  private def outlineNames(state: AppState): List[String] =
    state.surfaceById(outlineId).toList.flatMap {
      _.content match
        case SurfaceContent.Outline(symbols, _) => symbols.map(_.name)
        case _                                  => Nil
    }

  "A docked outline" should "pick up headings added by an edit" in {
    val stateManager = createStateManager("PanelContentRefreshSpec")
    stateManager
      .updateStateValidated(state =>
        DockedPanelFixtures.dock(
          withMarkdown(state, "# One\n"),
          outlineId,
          SurfaceContent.Outline(List(Symbol("One", SymbolKind.Heading, Location(0, 0)))),
          PanelPosition.Right,
          30
        )
      )
      .unsafeRunSync()

    stateManager.updateStateValidated(withMarkdown(_, "# One\n\n# Two\n")).unsafeRunSync()

    val refreshed = awaitState(stateManager)(outlineNames(_) == List("One", "Two")).unsafeRunSync()
    outlineNames(refreshed) shouldBe List("One", "Two")
  }
end PanelContentRefreshSpec
