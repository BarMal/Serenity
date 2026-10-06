package com.serenity

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import com.serenity.rope.Balance
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{FrameOutput, Java2DRenderSurface, RendererFramePlanner, ScreenIdentity}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** #1810: switching spell check on from the command runner must show the misspelling marks in the editor while the
  * runner is still open, not only once it closes. The marks come from diagnostics that arrive without moving a line, so
  * they must reach the screen with nothing covering the editor too. Frames are painted the way the GUI paints them: one
  * set of render caches across frames, alternating between two persisted backing images, with only the damage each
  * transition reports.
  */
class SpellCheckToggleUnderCommandRunnerSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val runnerId = SurfaceId("command-runner")

  private val codeFont = FontLoader
    .loadCodeFont(FontLoader.FontConfig(codeFontFamily = FontLoader.BundledCodeFontFamily, enableLigatures = true))
    .unsafeRunSync()

  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)
  private val viewport    = ViewportSize(90, 30)
  private val widthPx     = viewport.width * cellMetrics.charWidth
  private val heightPx    = viewport.height * cellMetrics.lineHeight

  private val buffer = Buffer
    .fromString(bufferId, "hello wurld\nsecond line\nthird line")
    .copy(editing = EditingState(List(CursorPosition(0, 0))))

  private val misspelling = Diagnostic(
    range = LspRange(LspPosition(0, 6), LspPosition(0, 11)),
    severity = Some(DiagnosticSeverity.Warning),
    message = "Possible spelling issue: wurld",
    source = Some(SpellChecker.Source),
    code = Some("unknown-word")
  )

  private def editorState(spellCheckEnabled: Boolean, layerCaching: Boolean, runnerShown: Boolean): AppState =
    val config = AppConfig.default
      .withLayerCaching(layerCaching)
      .withSpellCheck(SpellCheckConfig(enabled = spellCheckEnabled, languages = List("en")))
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, config)
      .updateSearchTerm("")(using
        CommandRegistry.default
      )
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = if runnerShown then Focus.Surface(runnerId) else Focus.EditorPane(paneId),
        theme = Theme.light,
        config = config
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = Option
          .when(runnerShown)(
            UiSurface(
              runnerId,
              SurfaceContent.CommandPalette(runner),
              SurfacePresentation.Floating(Some(CursorPosition(0, 0)), SurfacePlacement.BelowCursor)
            )
          )
          .toList
      )
    )

  private def withMisspelling(state: AppState): AppState =
    state.copy(runtime =
      state.runtime.copy(languageService =
        LanguageServiceState(diagnosticsState =
          DiagnosticsState(diagnostics = Map(SpellChecker.diagnosticsUri(buffer) -> List(misspelling)))
        )
      )
    )

  /** Frames painted the way the window paints them: one set of render caches, one screen, two backing images taken in
    * turn, and only the damage each transition reports.
    */
  final private class Frames:
    private val caches    = RenderCaches.create()
    private val screen    = ScreenIdentity(new Object)
    private val published = new AtomicReference[Option[BufferedImage]](None)
    private val spare     = new AtomicReference[Option[BufferedImage]](None)

    def paint(state: AppState, damage: Damage): BufferedImage =
      val image =
        spare.getAndSet(None).getOrElse(new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_ARGB))
      val captured = new AtomicReference[Option[BufferedImage]](None)
      val surface = new Java2DRenderSurface(
        image,
        cellMetrics,
        codeFont,
        out => captured.set(Option(out)),
        logicalWidthPx = widthPx,
        logicalHeightPx = heightPx,
        contentPersists = true
      )
      val _ = RendererFramePlanner.renderFrame(
        state,
        cursorVisible = false,
        surface,
        viewport,
        caches.authoritativeScene.forState(state, viewport, codeFont, codeFont),
        codeFont,
        codeFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        None,
        Some(FrameOutput(screen, new AtomicReference[Option[List[PixelRect]]](None))),
        damage,
        caches
      )
      val out = captured.get().getOrElse(image)
      published.getAndSet(Some(out)).filterNot(_ eq out).foreach(previous => spare.set(Some(previous)))
      out

  private def errorPixels(image: BufferedImage): Int =
    val error = Theme.light.error.foreground.argb
    (for
      y <- 0 until image.getHeight
      x <- 0 until image.getWidth
      if image.getRGB(x, y) == error
    yield 1).size

  for
    runnerShown  <- List(true, false)
    layerCaching <- List(false, true)
  do
    val setting = if runnerShown then "with the command runner open" else "with nothing over the editor"

    s"Enabling spell check $setting (layer caching $layerCaching)" should
      "mark the misspelling in the editor before the runner closes" in {
        val frames  = new Frames
        val off     = editorState(spellCheckEnabled = false, layerCaching, runnerShown)
        val toggled = editorState(spellCheckEnabled = true, layerCaching, runnerShown)
        val checked = withMisspelling(toggled)

        frames.paint(off, Damage.Everything)
        frames.paint(off, Damage.Nothing)
        frames.paint(toggled, DamageProducer.forTransition(off, toggled))
        val withMarks = frames.paint(checked, DamageProducer.forTransition(toggled, checked))

        errorPixels(withMarks) should be > 0
      }

    it should "clear the marks again when switched off before the runner closes" in {
      val frames  = new Frames
      val checked = withMisspelling(editorState(spellCheckEnabled = true, layerCaching, runnerShown))
      val toggled = editorState(spellCheckEnabled = false, layerCaching, runnerShown).copy(runtime = checked.runtime)
      val cleared = editorState(spellCheckEnabled = false, layerCaching, runnerShown)

      frames.paint(checked, Damage.Everything)
      frames.paint(checked, Damage.Nothing)
      frames.paint(toggled, DamageProducer.forTransition(checked, toggled))
      val withoutMarks = frames.paint(cleared, DamageProducer.forTransition(toggled, cleared))

      errorPixels(withoutMarks) shouldBe 0
    }
