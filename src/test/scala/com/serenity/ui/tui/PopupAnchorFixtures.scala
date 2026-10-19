package com.serenity.ui.tui

import java.awt.Font
import java.io.StringWriter

import com.serenity.TestWorkspaceTrees
import com.serenity.config.{AppConfig, HeadingsConfig}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, RendererCursorOverlay, RendererEntryPoints}

/** A terminal editor state with headings and a prose paragraph, rendered through the real paint path so a spec can
  * compare a popup's geometry with the cell the terminal's hardware cursor was actually placed on.
  */
object PopupAnchorFixtures:

  given Balance = Balance.default

  val viewport: ViewportSize = ViewportSize(60, 24)
  val paneId: PaneId         = PaneId(0)
  val bufferId: BufferId     = BufferId(1)

  private val font = Font(Font.MONOSPACED, Font.PLAIN, 12)

  val wrappedParagraph: String = Vector.fill(14)("abcdefgh").mkString(" ")

  val document: RichTextDocument = RichTextDocument(
    List(
      RichTextParagraph.plain("Opening line."),
      RichTextParagraph.plain("Headline", role = ParagraphRole.Heading(1)),
      RichTextParagraph.plain("Under the headline."),
      RichTextParagraph.plain("Subhead", role = ParagraphRole.Heading(2)),
      RichTextParagraph.plain(wrappedParagraph)
    )
  )

  val spacedHeadings: AppConfig =
    AppConfig.conventional
      .withLineNumbers(false)
      .withHeadingsConfig(
        HeadingsConfig()
          .withLevel(1)(_.copy(spaceBefore = Some(2.0f), spaceAfter = Some(1.0f)))
          .withLevel(2)(_.copy(spaceBefore = Some(2.0f), spaceAfter = Some(1.0f)))
      )

  def stateOf(config: AppConfig, cursor: CursorPosition, surfaces: List[UiSurface] = Nil): AppState =
    val base = Buffer.fromString(bufferId, document.plainText)
    val buffer = base.copy(
      richText = RichTextState(richTextDocument = Some(document)),
      editing = EditingState(List(cursor)),
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = viewport.width, visibleLines = viewport.height)
    )
    val initial = AppState.initial(AppConfig.conventional)
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId),
        config = config
      ),
      runtime = initial.runtime.copy(
        capabilities = FrontendCapabilities.tui(),
        viewportSize = Some(viewport),
        uiSurfaces = surfaces
      )
    )

  def floating(placement: SurfacePlacement, anchor: CursorPosition): UiSurface =
    UiSurface(
      SurfaceId("popup"),
      SurfaceContent.QuickInfo("hover text"),
      SurfacePresentation.Floating(Some(anchor), placement)
    )

  /** The terminal as painted, with the hardware cursor placed on the caret's cell. */
  def renderWithCaret(state: AppState): TerminalEmulator =
    val writer  = new StringWriter()
    val surface = new TerminalRenderSurface(viewport.width, viewport.height, writer, CellMetrics.cellUnit)
    val caches  = RenderCaches.create()
    val spec    = FontSpec.fromAwt(font)
    RendererEntryPoints.render(state, true, surface, viewport, spec, spec, CellMetrics.cellUnit, None, caches)
    surface.flush()
    val _ = RendererCursorOverlay.renderCursorOnly(
      state,
      true,
      surface,
      viewport,
      spec,
      spec,
      spec,
      CellMetrics.cellUnit,
      CellMetrics.cellUnit,
      None,
      caches
    )
    TerminalEmulator.blank(viewport.width, viewport.height).consume(writer.toString)
