package com.serenity.state.manager

import com.serenity.animation.{AnimatedCell, AnimationState, CharacterKey}
import com.serenity.config.RenderDamageGranularity
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import com.serenity.rope.{Balance, Rope}
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import com.serenity.ui.layout.{DirtyLineDiff, PanelPosition}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DamageProducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private def stateWithContent(text: String, cursors: List[CursorPosition] = List(CursorPosition(0, 0))): AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted.buffers(bufferId).document.copy(content = Rope(text)),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = cursors)
            )
        )
      )
    )

  /** A monospaced (`Code`) buffer under `Cells` granularity -- the one combination [[DamageProducer]] may report
    * column-precise damage for.
    */
  private def cellsEligibleState(text: String): AppState =
    val base = stateWithContent(text)
    base.copy(persisted =
      base.persisted.copy(
        config = base.persisted.config.withRenderDamageGranularity(RenderDamageGranularity.Cells),
        buffers = base.persisted.buffers.updated(
          bufferId,
          base.persisted
            .buffers(bufferId)
            .copy(document = base.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
        )
      )
    )

  "DamageProducer.forTransition" should "report no damage when nothing changed" in {
    val state = stateWithContent("alpha\nbeta\ngamma")
    DamageProducer.forTransition(state, state) shouldBe Damage.Nothing
  }

  it should "report the old and new cursor rows for a cursor move, plus Chrome since the active gutter shows it" in {
    val before = stateWithContent("alpha\nbeta\ngamma", cursors = List(CursorPosition(0, 0)))
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(editing = before.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(2, 3))))
        )
      )
    )

    DamageProducer.forTransition(before, after) shouldBe
      Damage.Combined(Set(Damage.BufferRows(bufferId, Set(0, 2)), Damage.Chrome))
  }

  it should "report no damage when a transition changes nothing about the cursors at all" in {
    val before = stateWithContent("alpha\nbeta\ngamma", cursors = List(CursorPosition(0, 0)))
    DamageProducer.forTransition(before, before) shouldBe Damage.Nothing
  }

  it should "report every old and new row for a multi-cursor move, plus Chrome since the active gutter shows it" in {
    val before = stateWithContent("alpha\nbeta\ngamma", cursors = List(CursorPosition(0, 0), CursorPosition(1, 0)))
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(editing =
              before.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(1, 2), CursorPosition(2, 0)))
            )
        )
      )
    )

    DamageProducer.forTransition(before, after) shouldBe
      Damage.Combined(Set(Damage.BufferRows(bufferId, Set(0, 1, 2)), Damage.Chrome))
  }

  it should "report the spanned rows for a selection change" in {
    val before = stateWithContent("first\nsecond\nthird\nfourth")
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(editing =
              before.persisted
                .buffers(bufferId)
                .editing
                .copy(selection = Some(Selection(CursorPosition(1, 0), CursorPosition(3, 2))))
            )
        )
      )
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(1, 2, 3))
  }

  it should "report both the old and new selection rows when a selection moves" in {
    val before = stateWithContent("first\nsecond\nthird\nfourth").copy()
    val withSelection = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(editing =
              before.persisted
                .buffers(bufferId)
                .editing
                .copy(selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 5))))
            )
        )
      )
    )
    val after = withSelection.copy(persisted =
      withSelection.persisted.copy(buffers =
        withSelection.persisted.buffers.updated(
          bufferId,
          withSelection.persisted
            .buffers(bufferId)
            .copy(editing =
              withSelection.persisted
                .buffers(bufferId)
                .editing
                .copy(selection = Some(Selection(CursorPosition(2, 0), CursorPosition(2, 5))))
            )
        )
      )
    )

    DamageProducer.forTransition(withSelection, after) shouldBe Damage.BufferRows(bufferId, Set(0, 2))
  }

  it should "report the spanned rows when a document comment is added" in {
    val before  = stateWithContent("first\nsecond\nthird\nfourth")
    val comment = DocumentComment(CursorPosition(1, 0), CursorPosition(2, 3), "note")
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(annotations = before.persisted.buffers(bufferId).annotations.copy(documentComments = List(comment)))
        )
      )
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(1, 2))
  }

  it should "report the spanned rows when an LSP diagnostic is added for the buffer's URI" in {
    val before = stateWithContent("first\nsecond\nthird\nfourth")
    val uri    = SpellChecker.diagnosticsUri(before.persisted.buffers(bufferId))
    val diagnostic = Diagnostic(
      LspRange(LspPosition(2, 0), LspPosition(2, 5)),
      Some(DiagnosticSeverity.Warning),
      "unused value",
      Some("benchmark")
    )
    val after = before.copy(runtime =
      before.runtime.copy(diagnosticsState =
        before.runtime.diagnosticsState
          .copy(diagnostics = before.runtime.diagnosticsState.diagnostics.updated(uri, List(diagnostic)))
      )
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(2))
  }

  it should "report the full buffer extent (plus Chrome, since the active gutter shows the language) on a language change" in {
    val before = stateWithContent("first\nsecond\nthird")
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(document = before.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
        )
      )
    )

    DamageProducer.forTransition(before, after) shouldBe
      Damage.Combined(Set(Damage.BufferRows(bufferId, Set(0, 1, 2)), Damage.Chrome))
  }

  it should
    "report the full buffer extent (plus Chrome, since the active gutter's line numbers follow it) on a scroll" in {
      val before = stateWithContent("first\nsecond\nthird")
      val after = before.copy(persisted =
        before.persisted.copy(buffers =
          before.persisted.buffers.updated(
            bufferId,
            before.persisted.buffers(bufferId).copy(viewport = Viewport.default.copy(topLine = 1))
          )
        )
      )

      DamageProducer.forTransition(before, after) shouldBe
        Damage.Combined(Set(Damage.BufferRows(bufferId, Set(0, 1, 2)), Damage.Chrome))
    }

  it should "report no damage from scrolling when the viewport does not actually change" in {
    val before = stateWithContent("first\nsecond\nthird")
    DamageProducer.forTransition(before, before) shouldBe Damage.Nothing
  }

  it should
    "report Everything when the syntax-highlighting setting toggles, since it recolors every buffer's own content" in {
      val before = stateWithContent("alpha")
      val after = before.copy(persisted =
        before.persisted.copy(config =
          before.persisted.config.withSyntaxHighlighting(
            !before.persisted.config.languageToolsConfig.syntaxHighlightingEnabled
          )
        )
      )

      DamageProducer.forTransition(before, after) shouldBe Damage.Everything
    }

  private val revealCell = AnimatedCell(Some('x'), List(java.awt.Color.WHITE), Nil)

  it should "report the changed rows when a character-reveal animation tick advances" in {
    val before   = stateWithContent("first\nsecond\nthird")
    val animated = AnimationState(Map(CharacterKey(0, 1) -> revealCell))

    DamageProducer.forTransition(
      before,
      before,
      beforeAnimations = Map.empty,
      afterAnimations = Map(bufferId -> animated)
    ) shouldBe Damage.BufferRows(bufferId, Set(1))
  }

  it should "report the union of changed rows when several cells across different rows tick at once" in {
    val before   = stateWithContent("first\nsecond\nthird")
    val animated = AnimationState(Map(CharacterKey(0, 0) -> revealCell, CharacterKey(2, 2) -> revealCell))

    DamageProducer.forTransition(
      before,
      before,
      beforeAnimations = Map.empty,
      afterAnimations = Map(bufferId -> animated)
    ) shouldBe Damage.BufferRows(bufferId, Set(0, 2))
  }

  it should "report no damage when a transition changes nothing about the buffer's animations" in {
    val before   = stateWithContent("first\nsecond\nthird")
    val animated = AnimationState(Map(CharacterKey(0, 1) -> revealCell))

    DamageProducer.forTransition(
      before,
      before,
      beforeAnimations = Map(bufferId -> animated),
      afterAnimations = Map(bufferId -> animated)
    ) shouldBe Damage.Nothing
  }

  it should "report Everything when a theme transition advances, since it cross-fades every visible glyph" in {
    val before = stateWithContent("alpha")
    val after = before.copy(runtime =
      before.runtime.copy(themeTransition =
        Some(ThemeTransition(before.persisted.theme, currentStep = 1, totalSteps = 10))
      )
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }

  it should "report Everything when a surface animation advances, since it composites through the full-render path" in {
    val before = stateWithContent("alpha")
    val after =
      before.copy(runtime =
        before.runtime.copy(surfaceAnimations =
          before.runtime.surfaceAnimations.updated(SurfaceId("palette"), SurfaceAnimationState())
        )
      )

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }

  it should "report Everything when a floating/pinned/modal surface appears" in {
    val before  = stateWithContent("alpha")
    val surface = UiSurface(SurfaceId("palette"), SurfaceContent.Comments(Nil), SurfacePresentation.Modal)
    val after   = before.copy(runtime = before.runtime.copy(uiSurfaces = surface :: before.runtime.uiSurfaces))

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }

  it should "report Damage.Surface scoped to a floating surface when only its own content changes (#1100 stage 3)" in {
    val surface = UiSurface(
      SurfaceId("palette"),
      SurfaceContent.Comments(Nil),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val bare   = stateWithContent("alpha")
    val before = bare.copy(runtime = bare.runtime.copy(uiSurfaces = List(surface)))
    val after = before.copy(runtime =
      before.runtime.copy(uiSurfaces = List(surface.copy(content = SurfaceContent.Diagnostics(Nil))))
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.Surface(SurfaceId("palette"))
  }

  it should "report Damage.Surface scoped to the modal when only its own content changes" in {
    val surface = UiSurface(SurfaceId("palette"), SurfaceContent.Comments(Nil), SurfacePresentation.Modal)
    val bare    = stateWithContent("alpha")
    val before  = bare.copy(runtime = bare.runtime.copy(uiSurfaces = List(surface)))
    val after = before.copy(runtime =
      before.runtime.copy(uiSurfaces = List(surface.copy(content = SurfaceContent.Diagnostics(Nil))))
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.Surface(SurfaceId("palette"))
  }

  it should "report Damage.Surface scoped to a pinned panel when only its own content changes (#1100 stage 3)" in {
    val surface = UiSurface(
      SurfaceId("outline"),
      SurfaceContent.Outline(Nil),
      SurfacePresentation.Pinned(PanelPosition.Left, 20)
    )
    val bare   = stateWithContent("alpha")
    val before = bare.copy(runtime = bare.runtime.copy(uiSurfaces = List(surface)))
    val after  = before.copy(runtime = before.runtime.copy(uiSurfaces = List(surface.copy(dismissOnMove = true))))

    DamageProducer.forTransition(before, after) shouldBe Damage.Surface(SurfaceId("outline"))
  }

  it should "report Damage.Surface scoped to an expanded panel when only its own content changes (#1100 stage 3)" in {
    val surface = UiSurface(
      SurfaceId("outline"),
      SurfaceContent.Outline(Nil),
      SurfacePresentation.Expanded(PanelPosition.Left, 20)
    )
    val bare   = stateWithContent("alpha")
    val before = bare.copy(runtime = bare.runtime.copy(uiSurfaces = List(surface)))
    val after  = before.copy(runtime = before.runtime.copy(uiSurfaces = List(surface.copy(dismissOnMove = true))))

    DamageProducer.forTransition(before, after) shouldBe Damage.Surface(SurfaceId("outline"))
  }

  it should "report Everything when the modal changes alongside another surface" in {
    val modal = UiSurface(SurfaceId("palette"), SurfaceContent.Comments(Nil), SurfacePresentation.Modal)
    val pinned = UiSurface(
      SurfaceId("outline"),
      SurfaceContent.Outline(Nil),
      SurfacePresentation.Pinned(PanelPosition.Left, 20)
    )
    val bare   = stateWithContent("alpha")
    val before = bare.copy(runtime = bare.runtime.copy(uiSurfaces = List(modal, pinned)))
    val after = before.copy(runtime =
      before.runtime.copy(uiSurfaces =
        List(modal.copy(content = SurfaceContent.Diagnostics(Nil)), pinned.copy(dismissOnMove = true))
      )
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }

  it should "report Everything when a surface's presentation changes away from Modal" in {
    val surface = UiSurface(SurfaceId("palette"), SurfaceContent.Comments(Nil), SurfacePresentation.Modal)
    val bare    = stateWithContent("alpha")
    val before  = bare.copy(runtime = bare.runtime.copy(uiSurfaces = List(surface)))
    val after = before.copy(runtime =
      before.runtime.copy(uiSurfaces =
        List(surface.copy(presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)))
      )
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }

  it should "report Everything when focus moves between two already-open floating surfaces" in {
    val bare   = stateWithContent("alpha")
    val before = bare.copy(persisted = bare.persisted.copy(focus = Focus.Surface(SurfaceId("a"))))
    val after  = before.copy(persisted = before.persisted.copy(focus = Focus.Surface(SurfaceId("b"))))

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }

  it should "report no damage from uiSurfaces or focus when neither changes" in {
    val surface = UiSurface(SurfaceId("palette"), SurfaceContent.Comments(Nil), SurfacePresentation.Modal)
    val bare    = stateWithContent("alpha")
    val state = bare.copy(
      persisted = bare.persisted.copy(focus = Focus.Surface(SurfaceId("palette"))),
      runtime = bare.runtime.copy(uiSurfaces = List(surface))
    )

    DamageProducer.forTransition(state, state) shouldBe Damage.Nothing
  }

  private val activePaneId = PaneId(0)

  it should "report PaneChrome damage when a buffer's dirty flag toggles" in {
    val before = stateWithContent("alpha")
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(document = before.persisted.buffers(bufferId).document.copy(isDirty = true))
        )
      )
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.PaneChrome(activePaneId)
  }

  it should "report PaneChrome and Chrome damage when a buffer's file path changes, since both header and gutter show it" in {
    val before = stateWithContent("alpha")
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(document =
              before.persisted.buffers(bufferId).document.copy(filePath = Some(java.nio.file.Path.of("a.txt")))
            )
        )
      )
    )

    DamageProducer.forTransition(before, after) shouldBe
      Damage.Combined(Set(Damage.PaneChrome(activePaneId), Damage.Chrome))
  }

  it should "report no PaneChrome damage when nothing header-relevant changed" in {
    val before = stateWithContent("alpha")
    val after =
      before.copy(persisted =
        before.persisted.copy(buffers =
          before.persisted.buffers.updated(
            bufferId,
            before.persisted
              .buffers(bufferId)
              .copy(document = before.persisted.buffers(bufferId).document.copy(isNewEmpty = true))
          )
        )
      )

    DamageProducer.forTransition(before, after) shouldBe Damage.Nothing
  }

  it should "report Everything when the layout changes, e.g. a pane is added" in {
    val before   = stateWithContent("alpha")
    val secondId = PaneId(1)
    val after = before.copy(persisted =
      before.persisted.copy(layout =
        before.persisted.layout.copy(
          editorPanes =
            before.persisted.layout.editorPanes.updated(secondId, EditorPane.withBuffer(secondId, bufferId)),
          paneOrder = before.persisted.layout.paneOrder :+ secondId
        )
      )
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }

  it should "combine BufferRows and Chrome damage when the active buffer's cursor moves, since the gutter shows it" in {
    val before = stateWithContent("first\nsecond\nthird", cursors = List(CursorPosition(0, 0)))
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(editing = before.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(1, 2))))
        )
      )
    )

    DamageProducer.forTransition(before, after) shouldBe
      Damage.Combined(Set(Damage.BufferRows(bufferId, Set(0, 1)), Damage.Chrome))
  }

  it should "report only BufferRows, no Chrome damage, when a non-active buffer's cursor moves" in {
    val otherId = BufferId(99)
    val bare    = stateWithContent("first\nsecond")
    val before = bare.copy(persisted =
      bare.persisted.copy(
        buffers = AppState.initial.persisted.buffers
          .updated(
            bufferId,
            AppState.initial.persisted
              .buffers(bufferId)
              .copy(document =
                AppState.initial.persisted.buffers(bufferId).document.copy(content = Rope("first\nsecond"))
              )
          ) +
          (otherId -> AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              id = otherId,
              document = AppState.initial.persisted.buffers(bufferId).document.copy(content = Rope("x\ny")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 0)))
            ))
      )
    )
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          otherId,
          before.persisted
            .buffers(otherId)
            .copy(editing = before.persisted.buffers(otherId).editing.copy(cursors = List(CursorPosition(1, 0))))
        )
      )
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(otherId, Set(0, 1))
  }

  it should "report the damaged row for a single-character edit on one line" in {
    val before = stateWithContent("alpha\nbeta\ngamma")
    val editedBuffer = before.persisted
      .buffers(bufferId)
      .copy(document =
        before.persisted
          .buffers(bufferId)
          .document
          .copy(content =
            before.persisted
              .buffers(bufferId)
              .document
              .content
              .insert(1, "X")
              .getOrElse(fail("expected insert to succeed"))
          )
      )
    val after =
      before.copy(persisted = before.persisted.copy(buffers = before.persisted.buffers.updated(bufferId, editedBuffer)))

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0))
  }

  it should "report every damaged row for an edit spanning a newline" in {
    val before = stateWithContent("alpha\nbeta\ngamma")
    val editedBuffer = before.persisted
      .buffers(bufferId)
      .copy(document =
        before.persisted
          .buffers(bufferId)
          .document
          .copy(content =
            before.persisted
              .buffers(bufferId)
              .document
              .content
              .insert(7, "X\nY")
              .getOrElse(fail("expected insert to succeed"))
          )
      )
    val after =
      before.copy(persisted = before.persisted.copy(buffers = before.persisted.buffers.updated(bufferId, editedBuffer)))

    // "alpha\nbeX\nYta\ngamma" -- the insertion at offset 7 (into "beta", the second line) spans into a new line.
    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(1, 2))
  }

  it should "report the deletion's line even though the deleted range is empty in the result" in {
    val before = stateWithContent("helloXworld")
    val editedBuffer = before.persisted
      .buffers(bufferId)
      .copy(document =
        before.persisted
          .buffers(bufferId)
          .document
          .copy(content =
            before.persisted
              .buffers(bufferId)
              .document
              .content
              .delete(5, 6)
              .getOrElse(fail("expected delete to succeed"))
          )
      )
    val after =
      before.copy(persisted = before.persisted.copy(buffers = before.persisted.buffers.updated(bufferId, editedBuffer)))

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0))
  }

  it should "report no damage for a buffer that did not exist before the transition" in {
    val before  = AppState.initial
    val otherId = BufferId(99)
    val after = before.copy(persisted =
      before.persisted.copy(
        buffers = before.persisted.buffers.updated(
          otherId,
          before.persisted
            .buffers(bufferId)
            .copy(id = otherId, document = before.persisted.buffers(bufferId).document.copy(content = Rope("new")))
        ),
        bufferOrder = before.persisted.bufferOrder :+ otherId
      )
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.Nothing
  }

  it should "report Everything when the theme changes, since it recolors every visible buffer's own content" in {
    val before = stateWithContent("alpha")
    val after = before.copy(persisted =
      before.persisted.copy(theme = if before.persisted.theme == Theme.dark then Theme.light else Theme.dark)
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }

  it should "report Everything when a theme change accompanies a content edit, since Everything subsumes it" in {
    val before = stateWithContent("alpha\nbeta")
    val editedBuffer = before.persisted
      .buffers(bufferId)
      .copy(document =
        before.persisted
          .buffers(bufferId)
          .document
          .copy(content =
            before.persisted
              .buffers(bufferId)
              .document
              .content
              .insert(0, "X")
              .getOrElse(fail("expected insert to succeed"))
          )
      )
    val edited =
      before.copy(persisted = before.persisted.copy(buffers = before.persisted.buffers.updated(bufferId, editedBuffer)))
    val after = edited.copy(persisted =
      edited.persisted.copy(theme = if before.persisted.theme == Theme.dark then Theme.light else Theme.dark)
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }

  it should "report BufferCells for a single-line edit on a monospaced buffer when granularity is Cells" in {
    val before = cellsEligibleState("alpha\nbeta\ngamma")
    val editedBuffer = before.persisted
      .buffers(bufferId)
      .copy(document =
        before.persisted
          .buffers(bufferId)
          .document
          .copy(content =
            before.persisted
              .buffers(bufferId)
              .document
              .content
              .insert(1, "X")
              .getOrElse(fail("expected insert to succeed"))
          )
      )
    val after =
      before.copy(persisted = before.persisted.copy(buffers = before.persisted.buffers.updated(bufferId, editedBuffer)))

    DamageProducer
      .forTransition(before, after) shouldBe Damage.BufferCells(bufferId, row = 0, fromColumn = 1, toColumn = Some(2))
  }

  it should "still report BufferRows under Cells granularity when the edit spans more than one row" in {
    val before = cellsEligibleState("alpha\nbeta\ngamma")
    val editedBuffer = before.persisted
      .buffers(bufferId)
      .copy(document =
        before.persisted
          .buffers(bufferId)
          .document
          .copy(content =
            before.persisted
              .buffers(bufferId)
              .document
              .content
              .insert(7, "X\nY")
              .getOrElse(fail("expected insert to succeed"))
          )
      )
    val after =
      before.copy(persisted = before.persisted.copy(buffers = before.persisted.buffers.updated(bufferId, editedBuffer)))

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(1, 2))
  }

  it should "still report BufferRows under Cells granularity for a prose buffer, since it may use measured layout" in {
    val bare = stateWithContent("alpha\nbeta\ngamma")
    val before = bare.copy(persisted =
      bare.persisted.copy(config =
        AppState.initial.persisted.config.withRenderDamageGranularity(RenderDamageGranularity.Cells)
      )
    )
    val editedBuffer = before.persisted
      .buffers(bufferId)
      .copy(document =
        before.persisted
          .buffers(bufferId)
          .document
          .copy(content =
            before.persisted
              .buffers(bufferId)
              .document
              .content
              .insert(1, "X")
              .getOrElse(fail("expected insert to succeed"))
          )
      )
    val after =
      before.copy(persisted = before.persisted.copy(buffers = before.persisted.buffers.updated(bufferId, editedBuffer)))

    before.persisted.buffers(bufferId).document.language shouldBe None
    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0))
  }

  it should "report BufferRows for a single-line monospaced edit when granularity is the Rows default" in {
    val base = stateWithContent("alpha\nbeta\ngamma")
    val before = base.copy(persisted =
      base.persisted.copy(buffers =
        base.persisted.buffers.updated(
          bufferId,
          base.persisted
            .buffers(bufferId)
            .copy(document = base.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
        )
      )
    )
    val editedBuffer = before.persisted
      .buffers(bufferId)
      .copy(document =
        before.persisted
          .buffers(bufferId)
          .document
          .copy(content =
            before.persisted
              .buffers(bufferId)
              .document
              .content
              .insert(1, "X")
              .getOrElse(fail("expected insert to succeed"))
          )
      )
    val after =
      before.copy(persisted = before.persisted.copy(buffers = before.persisted.buffers.updated(bufferId, editedBuffer)))

    before.persisted.config.surfaceConfig.renderDamageGranularity shouldBe RenderDamageGranularity.Rows
    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0))
  }

  "DamageProducer's reported rows" should "cover what DirtyLineDiff independently finds dirty for the same edit" in {
    val before = stateWithContent("first line\nsecond line\nthird line\nfourth line")
    val buffer = before.persisted.buffers(bufferId)
    val edited = buffer.copy(document =
      buffer.document.copy(content =
        buffer.document.content.insert(18, "-EDIT-").getOrElse(fail("expected insert to succeed"))
      )
    )
    val after =
      before.copy(persisted = before.persisted.copy(buffers = before.persisted.buffers.updated(bufferId, edited)))

    val font = com.serenity.ui.fonts.FontLoader.previewTextFont(after.persisted.config.editorConfig.fontConfig)
    val wrapPx =
      com.serenity.ui.layout.TextLayoutSnapshot.gridWrapWidthPx(80, after.persisted.config.editorConfig.fontConfig)
    val beforeSnapshot = com.serenity.ui.layout.TextLayoutSnapshot.fromBuffer(buffer, wrapPx, font)
    val afterSnapshot  = com.serenity.ui.layout.TextLayoutSnapshot.fromBuffer(edited, wrapPx, font)
    val dirty          = DirtyLineDiff.dirtyRows(Some(beforeSnapshot), afterSnapshot)

    val damage = DamageProducer.forTransition(before, after)
    dirty.subsetOf(Damage.coarsenToRows(bufferId, damage)) shouldBe true
  }

  "DamageProducer's focus-dimming coverage" should
    "report no damage from focus dimming when the feature is disabled, even across a paragraph boundary" in {
      val before = stateWithContent("first\nsecond\n\nfourth\nfifth", cursors = List(CursorPosition(0, 0)))
      val after = before.copy(persisted =
        before.persisted.copy(buffers =
          before.persisted.buffers.updated(
            bufferId,
            before.persisted
              .buffers(bufferId)
              .copy(editing = before.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(3, 0))))
          )
        )
      )

      before.persisted.config.surfaceConfig.focusedTextBodyEnabled shouldBe false
      DamageProducer.forTransition(before, after) shouldBe
        Damage.Combined(Set(Damage.BufferRows(bufferId, Set(0, 3)), Damage.Chrome))
    }

  it should "widen damage to every row whose dimmed state flips when the cursor crosses a paragraph boundary" in {
    val base   = stateWithContent("first\nsecond\n\nfourth\nfifth", cursors = List(CursorPosition(0, 0)))
    val before = base.copy(persisted = base.persisted.copy(config = base.persisted.config.withFocusedTextBody(true)))
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(editing = before.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(3, 0))))
        )
      )
    )

    DamageProducer.forTransition(before, after) shouldBe
      Damage.Combined(Set(Damage.BufferRows(bufferId, Set(0, 1, 3, 4)), Damage.Chrome))
  }

  it should "report no extra damage from focus dimming when the cursor stays within the same paragraph" in {
    val base   = stateWithContent("first\nsecond\n\nfourth\nfifth", cursors = List(CursorPosition(0, 0)))
    val before = base.copy(persisted = base.persisted.copy(config = base.persisted.config.withFocusedTextBody(true)))
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(editing = before.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(1, 0))))
        )
      )
    )

    DamageProducer.forTransition(before, after) shouldBe
      Damage.Combined(Set(Damage.BufferRows(bufferId, Set(0, 1)), Damage.Chrome))
  }

  it should "report Everything when the focused-text-body feature is toggled on, via chromeDamage's config check" in {
    val before = stateWithContent("first\nsecond\n\nfourth\nfifth", cursors = List(CursorPosition(0, 0)))
    val after =
      before.copy(persisted = before.persisted.copy(config = before.persisted.config.withFocusedTextBody(true)))

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }

  it should "report Everything when the focused-text-body feature is toggled off, via chromeDamage's config check" in {
    val base   = stateWithContent("first\nsecond\n\nfourth\nfifth", cursors = List(CursorPosition(0, 0)))
    val before = base.copy(persisted = base.persisted.copy(config = base.persisted.config.withFocusedTextBody(true)))
    val after =
      before.copy(persisted = before.persisted.copy(config = before.persisted.config.withFocusedTextBody(false)))

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }
