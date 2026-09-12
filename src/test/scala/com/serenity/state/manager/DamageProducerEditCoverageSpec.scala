package com.serenity.state.manager

import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{CursorInfoBarPlacement, CursorInfoBarSegment, RenderDamageGranularity}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.ui.layout.DirtyLineDiff
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DamageProducerEditCoverageSpec extends AnyFlatSpec with Matchers:

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

  "DamageProducer.forTransition" should "combine BufferRows and Chrome damage when the active buffer's cursor moves, since the gutter shows it" in {
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

  private def moveCursorTo(state: AppState, cursor: CursorPosition): AppState =
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(
          bufferId,
          state.persisted
            .buffers(bufferId)
            .copy(editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(cursor)))
        )
      )
    )

  private def withFloatingInfoBar(state: AppState): AppState =
    state.copy(persisted =
      state.persisted.copy(config =
        state.persisted.config
          .withCursorInfoBarSegments(List(CursorInfoBarSegment.Position))
          .withCursorInfoBarPlacement(CursorInfoBarPlacement.Floating)
      )
    )

  "DamageProducer's cursor-info-bar coverage" should
    "report Damage.Surface(cursor-info-bar) when the cursor moves with a Floating info bar enabled" in {
      // The Floating info bar is a *derived* surface (AppState.cursorInfoBarSurface), never stored in
      // runtime.uiSurfaces, so its cursor-following movement is invisible to uiSurfacesDamage. Without a Surface fact
      // the renderer's bounded repaint never pushes the rows it vacated, leaving a stale-background trail (#1263/#1265
      // regression fixed here).
      val before = withFloatingInfoBar(stateWithContent("alpha\nbeta\ngamma", cursors = List(CursorPosition(0, 0))))
      val after  = moveCursorTo(before, CursorPosition(1, 0))

      Damage.surfaceIds(DamageProducer.forTransition(before, after)) should contain(UiSurface.CursorInfoBarSurfaceId)
    }

  it should "not report Damage.Surface(cursor-info-bar) when the info bar is pinned to the bottom" in {
    val base = stateWithContent("alpha\nbeta\ngamma", cursors = List(CursorPosition(0, 0)))
    val before = base.copy(persisted =
      base.persisted.copy(config =
        base.persisted.config
          .withCursorInfoBarSegments(List(CursorInfoBarSegment.Position))
          .withCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom)
      )
    )
    val after = moveCursorTo(before, CursorPosition(1, 0))

    Damage.surfaceIds(DamageProducer.forTransition(before, after)) should not contain UiSurface.CursorInfoBarSurfaceId
  }

  it should "not report Damage.Surface(cursor-info-bar) when no info-bar segments are configured" in {
    val before = stateWithContent("alpha\nbeta\ngamma", cursors = List(CursorPosition(0, 0)))
    val after  = moveCursorTo(before, CursorPosition(1, 0))

    before.persisted.config.cursorInfoBarSegments shouldBe Nil
    Damage.surfaceIds(DamageProducer.forTransition(before, after)) should not contain UiSurface.CursorInfoBarSurfaceId
  }
