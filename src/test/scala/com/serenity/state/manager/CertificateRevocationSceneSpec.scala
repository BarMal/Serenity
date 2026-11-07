package com.serenity.state.manager

import java.util.concurrent.Executor

import com.serenity.config.AppConfig
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A row-count certificate the layout contradicts (#2447 B2): the scene builder hands the rows it painted to the count,
  * and a certificate that called a painted-as-wrapped line one row is revoked and every line is counted again by the
  * wrap, so the pane's scroll extent is the wrap's and not the certificate's.
  */
class CertificateRevocationSceneSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val viewportSize     = ViewportSize(100, 30)
  private val paneId           = PaneId(0)
  private val frc              = TextLayoutSnapshot.defaultFontRenderContext()
  private val config           = AppConfig.conventional
  private val inline: Executor = _.run()
  private val never: Executor  = _ => ()

  private val codeFont       = FontLoader.previewFontForRole(config.editorConfig.fontConfig, TypographyRole.Code)
  private val textFont       = FontLoader.previewFontForRole(config.editorConfig.fontConfig, TypographyRole.Prose)
  private val certificateKey = NominalAdvances.Key(codeFont, frc)

  // Far wider than the pane, so each wraps to several rows, yet a twentieth of that width fits it in one.
  private val lines = Vector.tabulate(120)(line => f"line $line%03d " + "ab -> cd " * 60)

  private def state: AppState =
    val plain   = Buffer.fromString(BufferId(1), lines.mkString("\n"))
    val buffer  = plain.copy(document = plain.document.copy(language = Some(LanguageId.Scala)))
    val initial = AppState.initial(config)
    val laidOut = initial.copy(
      runtime = initial.runtime.copy(viewportSize = Some(viewportSize)),
      persisted = initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = config
      )
    )
    LayoutEngine.syncViewportDimensions(laidOut, viewportSize)

  private def totalRows(cache: WrappedLineCache): Int =
    AuthoritativeUiScene(cache)
      .forState(state, viewportSize, codeFont, textFont)
      .paneScrolls
      .getOrElse(paneId, fail("expected a scroll"))
      .extent
      .totalRows

  private val wrapOnly: Int = totalRows(WrappedLineCache.bounded(layoutMemos = LayoutMemos(never)))

  "A certificate that undercounts what the layout paints" should "be revoked by the scene that paints the lines" in {
    val memos = LayoutMemos(inline)
    memos.rowCertificates.of(certificateKey)
    val honest = memos.rowCertificates.of(certificateKey).value
    memos.rowCertificates.plant(certificateKey, honest.scaled(0.05f))

    totalRows(WrappedLineCache.bounded(layoutMemos = memos))

    memos.rowCertificates.of(certificateKey) shouldBe None
  }

  it should "leave the pane's scroll extent as the wrap counts it, off-screen lines included" in {
    val memos = LayoutMemos(inline)
    memos.rowCertificates.of(certificateKey)
    memos.rowCertificates.plant(certificateKey, memos.rowCertificates.of(certificateKey).value.scaled(0.05f))

    totalRows(WrappedLineCache.bounded(layoutMemos = memos)) shouldBe wrapOnly
    wrapOnly should be > lines.size
  }

  "A certificate that agrees with what the layout paints" should "survive the scene and give the wrap's extent" in {
    val memos = LayoutMemos(inline)
    memos.rowCertificates.of(certificateKey)
    memos.rowCertificates.of(certificateKey) shouldBe defined

    totalRows(WrappedLineCache.bounded(layoutMemos = memos)) shouldBe wrapOnly

    memos.rowCertificates.of(certificateKey) shouldBe defined
  }
