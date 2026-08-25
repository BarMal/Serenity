package com.serenity.state.manager

import com.serenity.animation.{AnimatedCell, AnimationState, CharacterKey}
import com.serenity.config.RenderDamageGranularity
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import com.serenity.rope.{Balance, Rope}
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import com.serenity.ui.layout.DirtyLineDiff
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DamageProducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private def stateWithContent(text: String, cursors: List[CursorPosition] = List(CursorPosition(0, 0))): AppState =
    AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(content = Rope(text), cursors = cursors)
      )
    )

  /** A monospaced (`Code`) buffer under `Cells` granularity -- the one combination [[DamageProducer]] may report
    * column-precise damage for.
    */
  private def cellsEligibleState(text: String): AppState =
    val base = stateWithContent(text)
    base.copy(
      config = base.config.withRenderDamageGranularity(RenderDamageGranularity.Cells),
      buffers = base.buffers.updated(bufferId, base.buffers(bufferId).copy(language = Some(LanguageId.Scala)))
    )

  "DamageProducer.forTransition" should "report no damage when nothing changed" in {
    val state = stateWithContent("alpha\nbeta\ngamma")
    DamageProducer.forTransition(state, state) shouldBe Damage.Nothing
  }

  it should "report the old and new cursor rows for a cursor move, even though content is unchanged" in {
    val before = stateWithContent("alpha\nbeta\ngamma", cursors = List(CursorPosition(0, 0)))
    val after = before.copy(buffers =
      before.buffers.updated(bufferId, before.buffers(bufferId).copy(cursors = List(CursorPosition(2, 3))))
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0, 2))
  }

  it should "report no damage when a transition changes nothing about the cursors at all" in {
    val before = stateWithContent("alpha\nbeta\ngamma", cursors = List(CursorPosition(0, 0)))
    DamageProducer.forTransition(before, before) shouldBe Damage.Nothing
  }

  it should "report every old and new row for a multi-cursor move" in {
    val before = stateWithContent("alpha\nbeta\ngamma", cursors = List(CursorPosition(0, 0), CursorPosition(1, 0)))
    val after = before.copy(buffers =
      before.buffers.updated(
        bufferId,
        before.buffers(bufferId).copy(cursors = List(CursorPosition(1, 2), CursorPosition(2, 0)))
      )
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0, 1, 2))
  }

  it should "report the spanned rows for a selection change" in {
    val before = stateWithContent("first\nsecond\nthird\nfourth")
    val after = before.copy(buffers =
      before.buffers.updated(
        bufferId,
        before.buffers(bufferId).copy(selection = Some(Selection(CursorPosition(1, 0), CursorPosition(3, 2))))
      )
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(1, 2, 3))
  }

  it should "report both the old and new selection rows when a selection moves" in {
    val before = stateWithContent("first\nsecond\nthird\nfourth").copy()
    val withSelection = before.copy(buffers =
      before.buffers.updated(
        bufferId,
        before.buffers(bufferId).copy(selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 5))))
      )
    )
    val after = withSelection.copy(buffers =
      withSelection.buffers.updated(
        bufferId,
        withSelection.buffers(bufferId).copy(selection = Some(Selection(CursorPosition(2, 0), CursorPosition(2, 5))))
      )
    )

    DamageProducer.forTransition(withSelection, after) shouldBe Damage.BufferRows(bufferId, Set(0, 2))
  }

  it should "report the spanned rows when a document comment is added" in {
    val before  = stateWithContent("first\nsecond\nthird\nfourth")
    val comment = DocumentComment(CursorPosition(1, 0), CursorPosition(2, 3), "note")
    val after = before.copy(buffers =
      before.buffers.updated(bufferId, before.buffers(bufferId).copy(documentComments = List(comment)))
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(1, 2))
  }

  it should "report the spanned rows when an LSP diagnostic is added for the buffer's URI" in {
    val before = stateWithContent("first\nsecond\nthird\nfourth")
    val uri    = SpellChecker.diagnosticsUri(before.buffers(bufferId))
    val diagnostic = Diagnostic(
      LspRange(LspPosition(2, 0), LspPosition(2, 5)),
      Some(DiagnosticSeverity.Warning),
      "unused value",
      Some("benchmark")
    )
    val after = before.copy(diagnostics = before.diagnostics.updated(uri, List(diagnostic)))

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(2))
  }

  it should "report the full buffer extent when its language changes, since that changes syntax highlighting" in {
    val before = stateWithContent("first\nsecond\nthird")
    val after = before.copy(buffers =
      before.buffers.updated(bufferId, before.buffers(bufferId).copy(language = Some(LanguageId.Scala)))
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0, 1, 2))
  }

  it should "report Chrome damage when the syntax-highlighting setting toggles, since it recolors every buffer" in {
    val before = stateWithContent("alpha")
    val after  = before.copy(config = before.config.withSyntaxHighlighting(!before.config.syntaxHighlightingEnabled))

    DamageProducer.forTransition(before, after) shouldBe Damage.Chrome
  }

  private val revealCell = AnimatedCell(Some('x'), List(java.awt.Color.WHITE), Nil)

  it should "report the changed rows when a character-reveal animation tick advances" in {
    val before = stateWithContent("first\nsecond\nthird")
    val animated = before
      .buffers(bufferId)
      .copy(
        animations = AnimationState(Map(CharacterKey(0, 1) -> revealCell))
      )
    val after = before.copy(buffers = before.buffers.updated(bufferId, animated))

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(1))
  }

  it should "report the union of changed rows when several cells across different rows tick at once" in {
    val before = stateWithContent("first\nsecond\nthird")
    val animated = before
      .buffers(bufferId)
      .copy(
        animations = AnimationState(Map(CharacterKey(0, 0) -> revealCell, CharacterKey(2, 2) -> revealCell))
      )
    val after = before.copy(buffers = before.buffers.updated(bufferId, animated))

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0, 2))
  }

  it should "report no damage when a transition changes nothing about the buffer's animations" in {
    val before = stateWithContent("first\nsecond\nthird")
    val animated = before
      .buffers(bufferId)
      .copy(
        animations = AnimationState(Map(CharacterKey(0, 1) -> revealCell))
      )
    val withAnimation = before.copy(buffers = before.buffers.updated(bufferId, animated))

    DamageProducer.forTransition(withAnimation, withAnimation) shouldBe Damage.Nothing
  }

  it should "report Everything when a theme transition advances, since it cross-fades every visible glyph" in {
    val before = stateWithContent("alpha")
    val after  = before.copy(themeTransition = Some(ThemeTransition(before.theme, currentStep = 1, totalSteps = 10)))

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }

  it should "report Everything when a surface animation advances, since it composites through the full-render path" in {
    val before = stateWithContent("alpha")
    val after =
      before.copy(surfaceAnimations = before.surfaceAnimations.updated(SurfaceId("palette"), SurfaceAnimationState()))

    DamageProducer.forTransition(before, after) shouldBe Damage.Everything
  }

  it should "report the damaged row for a single-character edit on one line" in {
    val before       = stateWithContent("alpha\nbeta\ngamma")
    val editedBuffer = before.buffers(bufferId).copy(content = before.buffers(bufferId).content.insert(1, "X"))
    val after        = before.copy(buffers = before.buffers.updated(bufferId, editedBuffer))

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0))
  }

  it should "report every damaged row for an edit spanning a newline" in {
    val before       = stateWithContent("alpha\nbeta\ngamma")
    val editedBuffer = before.buffers(bufferId).copy(content = before.buffers(bufferId).content.insert(7, "X\nY"))
    val after        = before.copy(buffers = before.buffers.updated(bufferId, editedBuffer))

    // "alpha\nbeX\nYta\ngamma" -- the insertion at offset 7 (into "beta", the second line) spans into a new line.
    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(1, 2))
  }

  it should "report the deletion's line even though the deleted range is empty in the result" in {
    val before       = stateWithContent("helloXworld")
    val editedBuffer = before.buffers(bufferId).copy(content = before.buffers(bufferId).content.delete(5, 6))
    val after        = before.copy(buffers = before.buffers.updated(bufferId, editedBuffer))

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0))
  }

  it should "report no damage for a buffer that did not exist before the transition" in {
    val before  = AppState.initial
    val otherId = BufferId(99)
    val after = before.copy(
      buffers = before.buffers.updated(otherId, before.buffers(bufferId).copy(id = otherId, content = Rope("new"))),
      bufferOrder = before.bufferOrder :+ otherId
    )

    DamageProducer.forTransition(before, after) shouldBe Damage.Nothing
  }

  it should "report Chrome damage when the theme changes" in {
    val before = stateWithContent("alpha")
    val after  = before.copy(theme = if before.theme == Theme.dark then Theme.light else Theme.dark)

    DamageProducer.forTransition(before, after) shouldBe Damage.Chrome
  }

  it should "combine content and chrome damage when both change together" in {
    val before       = stateWithContent("alpha\nbeta")
    val editedBuffer = before.buffers(bufferId).copy(content = before.buffers(bufferId).content.insert(0, "X"))
    val after = before
      .copy(buffers = before.buffers.updated(bufferId, editedBuffer))
      .copy(theme = if before.theme == Theme.dark then Theme.light else Theme.dark)

    DamageProducer.forTransition(before, after) shouldBe
      Damage.Combined(Set(Damage.BufferRows(bufferId, Set(0)), Damage.Chrome))
  }

  it should "report BufferCells for a single-line edit on a monospaced buffer when granularity is Cells" in {
    val before       = cellsEligibleState("alpha\nbeta\ngamma")
    val editedBuffer = before.buffers(bufferId).copy(content = before.buffers(bufferId).content.insert(1, "X"))
    val after        = before.copy(buffers = before.buffers.updated(bufferId, editedBuffer))

    DamageProducer
      .forTransition(before, after) shouldBe Damage.BufferCells(bufferId, row = 0, fromColumn = 1, toColumn = Some(2))
  }

  it should "still report BufferRows under Cells granularity when the edit spans more than one row" in {
    val before       = cellsEligibleState("alpha\nbeta\ngamma")
    val editedBuffer = before.buffers(bufferId).copy(content = before.buffers(bufferId).content.insert(7, "X\nY"))
    val after        = before.copy(buffers = before.buffers.updated(bufferId, editedBuffer))

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(1, 2))
  }

  it should "still report BufferRows under Cells granularity for a prose buffer, since it may use measured layout" in {
    val before = stateWithContent("alpha\nbeta\ngamma").copy(config =
      AppState.initial.config.withRenderDamageGranularity(RenderDamageGranularity.Cells)
    )
    val editedBuffer = before.buffers(bufferId).copy(content = before.buffers(bufferId).content.insert(1, "X"))
    val after        = before.copy(buffers = before.buffers.updated(bufferId, editedBuffer))

    before.buffers(bufferId).language shouldBe None
    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0))
  }

  it should "report BufferRows for a single-line monospaced edit when granularity is the Rows default" in {
    val base = stateWithContent("alpha\nbeta\ngamma")
    val before = base.copy(buffers =
      base.buffers.updated(bufferId, base.buffers(bufferId).copy(language = Some(LanguageId.Scala)))
    )
    val editedBuffer = before.buffers(bufferId).copy(content = before.buffers(bufferId).content.insert(1, "X"))
    val after        = before.copy(buffers = before.buffers.updated(bufferId, editedBuffer))

    before.config.surfaceConfig.renderDamageGranularity shouldBe RenderDamageGranularity.Rows
    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0))
  }

  "DamageProducer's reported rows" should "cover what DirtyLineDiff independently finds dirty for the same edit" in {
    val before = stateWithContent("first line\nsecond line\nthird line\nfourth line")
    val buffer = before.buffers(bufferId)
    val edited = buffer.copy(content = buffer.content.insert(18, "-EDIT-"))
    val after  = before.copy(buffers = before.buffers.updated(bufferId, edited))

    val font           = com.serenity.ui.fonts.FontLoader.previewTextFont(after.config.fontConfig)
    val wrapPx         = com.serenity.ui.layout.TextLayoutSnapshot.gridWrapWidthPx(80, after.config.fontConfig)
    val beforeSnapshot = com.serenity.ui.layout.TextLayoutSnapshot.fromBuffer(buffer, wrapPx, font)
    val afterSnapshot  = com.serenity.ui.layout.TextLayoutSnapshot.fromBuffer(edited, wrapPx, font)
    val dirty          = DirtyLineDiff.dirtyRows(Some(beforeSnapshot), afterSnapshot)

    val damage = DamageProducer.forTransition(before, after)
    dirty.subsetOf(Damage.coarsenToRows(bufferId, damage)) shouldBe true
  }
