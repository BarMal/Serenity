package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, HeadingsConfig}
import com.serenity.keystroke.events.{InsertChar, PageDown, PageUp}
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun}
import com.serenity.setCursorPosition
import com.serenity.state.manager.StateManagerTestFacade.updateState
import com.serenity.state.manager.{DamageProducer, RenderCaches, StateManager}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, PixelRect, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, Java2DRenderSurface, RendererFramePlanner, ScreenIdentity}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Headings with space above and below them keep the frames the editor draws incrementally equal to a cold repaint:
  * typing in a paragraph above or inside a heading, scrolling by rows, and paging through the document, over two images
  * that alternate as the window's pool does and a window that shows only the rects each frame names.
  */
class HeadingSpacingFramesSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont    = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)

  final private case class Window(width: Int, height: Int):
    def viewport: ViewportSize = ViewportSize(width / cellMetrics.charWidth, height / cellMetrics.lineHeight)

  private val fullHd = Window(1920, 1080)
  private val small  = Window(800, 600)

  private val spacing = HeadingsConfig()
    .withLevel(1)(_.copy(spaceBefore = Some(1.5f), spaceAfter = Some(0.75f)))
    .withLevel(2)(_.copy(spaceBefore = Some(1.0f), spaceAfter = Some(0.5f)))

  private val spacedConfig   = AppConfig.default.withHeadingsConfig(spacing)
  private val spacedChrome   = AppConfig.conventional.withHeadingsConfig(spacing)
  private val numberedConfig = spacedConfig.withLineNumbers(true)

  private def paragraph(text: String, role: ParagraphRole = ParagraphRole.Body): RichTextParagraph =
    RichTextParagraph(List(RichTextRun(text)), role = role)

  private def chaptered(chapters: Int, sections: Int): RichTextDocument =
    RichTextDocument(
      (1 to chapters).toList.flatMap { chapter =>
        paragraph(s"Chapter $chapter", ParagraphRole.Heading(1)) ::
          paragraph(s"Chapter $chapter opens. " + "lorem ipsum dolor sit amet " * 14) ::
          (1 to sections).toList.flatMap { section =>
            List(
              paragraph(
                s"Section $chapter.$section with a title long enough to wrap onto a second row of the page " * 2,
                ParagraphRole.Heading(2)
              ),
              paragraph(s"Chapter $chapter section $section. " + "the mage said nothing " * (6 + section * 5 % 17)),
              paragraph(s"More of section $section. " + "and watched the rain fall " * 9)
            )
          }
      }
    )

  final private class Pooled(window: Window, scale: Double, persists: Boolean = true):

    private val images = Vector.fill(2)(
      new BufferedImage(
        math.ceil(window.width * scale).toInt,
        math.ceil(window.height * scale).toInt,
        BufferedImage.TYPE_INT_RGB
      )
    )

    private val screenImage = new BufferedImage(images.head.getWidth, images.head.getHeight, BufferedImage.TYPE_INT_RGB)
    private val caches      = RenderCaches.create()
    private val screen      = ScreenIdentity(this)
    private val drawn       = AtomicInteger(0)

    def render(state: AppState, damage: Damage): BufferedImage =
      val image = images(drawn.getAndIncrement() % images.size)
      val surface = new Java2DRenderSurface(
        image,
        cellMetrics,
        codeFont,
        _ => (),
        logicalWidthPx = window.width,
        logicalHeightPx = window.height,
        deviceScaleX = scale,
        deviceScaleY = scale,
        contentPersists = persists,
        layerCacheOwnerOverride = Some(screen)
      )
      val repaintRegion = new AtomicReference[Option[List[PixelRect]]](None)
      val _ = RendererFramePlanner.renderFrame(
        state,
        cursorVisible = false,
        surface,
        window.viewport,
        caches.authoritativeScene.forState(state, window.viewport, codeFont, textFont),
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        None,
        Some(FrameOutput(screen, repaintRegion)),
        damage,
        caches = caches
      )
      present(image, repaintRegion.get())
      screenImage

    private def present(image: BufferedImage, region: Option[List[PixelRect]]): Unit =
      val g = screenImage.createGraphics()
      try
        region match
          case None => g.drawImage(image, 0, 0, null)
          case Some(rects) =>
            rects.foreach { rect =>
              val (x, y) = (math.floor(rect.xPx * scale).toInt, math.floor(rect.yPx * scale).toInt)
              val (w, h) = (math.ceil(rect.widthPx * scale).toInt, math.ceil(rect.heightPx * scale).toInt)
              val clipped = new java.awt.Rectangle(x, y, w, h)
                .intersection(new java.awt.Rectangle(0, 0, image.getWidth, image.getHeight))
              if !clipped.isEmpty then
                g.drawImage(
                  image.getSubimage(clipped.x, clipped.y, clipped.width, clipped.height),
                  clipped.x,
                  clipped.y,
                  null
                )
            }
      finally g.dispose()

  private def differing(window: Window, drawn: BufferedImage, state: AppState, scale: Double): Seq[(Int, Int)] =
    val cold = Pooled(window, scale, persists = false).render(state, Damage.Everything)
    for
      x <- 0 until drawn.getWidth
      y <- 0 until drawn.getHeight
      if (drawn.getRGB(x, y) & 0xffffff) != (cold.getRGB(x, y) & 0xffffff)
    yield (x, y)

  private def summary(d: Seq[(Int, Int)]): String =
    val ys = d.map(_._2)
    s"${d.size} px differ, y ${ys.minOption.getOrElse(0)}-${ys.maxOption.getOrElse(0)}"

  final private class Session(document: RichTextDocument, config: AppConfig, window: Window, scale: Double):
    private val (stateManager, id) =
      LaptopFrameBenchmarks.stateManagerHolding(document.plainText, config, grid = window.viewport)
    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(buffers =
            state.persisted.buffers.updatedWith(id)(
              _.map(b => b.copy(richText = b.richText.withSyncedDocument(Some(document), b.document.contentVersion)))
            )
          )
        )
      )
      .unsafeRunSync()

    private val paneId = stateManager.getCurrentState.unsafeRunSync().persisted.layout.editorPanes.keys.head
    private val frames = Pooled(window, scale)
    private val shown  = new AtomicReference(stateManager.getCurrentState.unsafeRunSync())
    frames.render(shown.get, Damage.Everything)

    /** Applies `step`, draws the frame it leaves with the damage the transition names, and reports where it differs
      * from a cold frame of the same state.
      */
    def after(step: StateManager => Unit): Seq[(Int, Int)] =
      step(stateManager)
      val next  = stateManager.getCurrentState.unsafeRunSync()
      val image = frames.render(next, DamageProducer.forTransition(shown.getAndSet(next), next))
      differing(window, image, next, scale)

    def placeCursor(line: Int, column: Int): Unit =
      stateManager.setCursorPosition(paneId, line, column).unsafeRunSync()

    def scrollTo(topLine: Int): Unit =
      stateManager
        .updateState(state =>
          state.copy(persisted =
            state.persisted.copy(buffers =
              state.persisted.buffers.updatedWith(id)(_.map(b => b.copy(viewport = b.viewport.copy(topLine = topLine))))
            )
          )
        )
        .unsafeRunSync()

  private def sessionFor(
    document: RichTextDocument,
    config: AppConfig = spacedConfig,
    window: Window = fullHd,
    scale: Double = 1.0
  ) = Session(document, config, window, scale)

  private val document = chaptered(4, 3)

  private def lineOf(prefix: String): Int =
    document.plainText.linesIterator.indexWhere(_.startsWith(prefix))

  for
    scale                <- List(1.0, 2.0)
    (windowName, window) <- List("1920x1080" -> fullHd, "800x600" -> small)
  do
    s"Typing into a paragraph above a spaced heading at ${scale}x in a $windowName window" should
      "draw what a cold frame draws" in {
        val session = sessionFor(document, window = window, scale = scale)
        session.placeCursor(1, 10)

        (1 to 4).flatMap(_ => session.after(_.applyEvent(InsertChar('x')).unsafeRunSync())) shouldBe empty
      }

    s"Typing into a spaced heading until it wraps at ${scale}x in a $windowName window" should
      "draw what a cold frame draws, moving the rows below it" in {
        val session = sessionFor(document, window = window, scale = scale)
        session.placeCursor(0, 3)

        val found = (1 to 6).flatMap { _ =>
          session.after(manager => (1 to 20).foreach(_ => manager.applyEvent(InsertChar('w')).unsafeRunSync()))
        }
        withClue(summary(found))(found shouldBe empty)
      }

    s"Typing into a body paragraph under a spaced heading at ${scale}x in a $windowName window" should
      "draw what a cold frame draws" in {
        val session = sessionFor(document, window = window, scale = scale)
        session.placeCursor(lineOf("Chapter 1 section 1"), 4)

        (1 to 4).flatMap(_ => session.after(_.applyEvent(InsertChar('q')).unsafeRunSync())) shouldBe empty
      }

    s"Scrolling past spaced headings at ${scale}x in a $windowName window" should
      "draw what a cold frame draws at every row" in {
        val session = sessionFor(document, window = window, scale = scale)

        val found = (1 to 24).flatMap(top => session.after(_ => session.scrollTo(top)))
        found shouldBe empty
      }

    s"Scrolling back up through spaced headings at ${scale}x in a $windowName window" should
      "draw what a cold frame draws at every row" in {
        val session = sessionFor(document, window = window, scale = scale)
        session.scrollTo(24)
        session.after(_ => ())

        (23 to 0 by -1).flatMap(top => session.after(_ => session.scrollTo(top))) shouldBe empty
      }

    s"PageDown and PageUp through a spaced document at ${scale}x in a $windowName window" should
      "draw what a cold frame draws at every step" in {
        val session = sessionFor(document, spacedChrome, window = window, scale = scale)

        val down = (1 to 14).flatMap(_ => session.after(_.applyEvent(PageDown).unsafeRunSync()))
        val up   = (1 to 14).flatMap(_ => session.after(_.applyEvent(PageUp).unsafeRunSync()))
        withClue(s"down ${summary(down)}: ")(down shouldBe empty)
        withClue(s"up ${summary(up)}: ")(up shouldBe empty)
      }

  "Line numbers beside headings without any space" should "be drawn as a cold frame draws them, typing" in {
    val session = sessionFor(document, numberedConfig.withHeadingsConfig(HeadingsConfig()))
    session.placeCursor(3, 2)

    (1 to 3).flatMap(_ => session.after(_.applyEvent(InsertChar('n')).unsafeRunSync())) shouldBe empty
  }

  "Line numbers beside spaced headings" should "be drawn as a cold frame draws them while typing" in {
    val session = sessionFor(document, numberedConfig)
    session.placeCursor(3, 2)

    (1 to 3).flatMap(_ => session.after(_.applyEvent(InsertChar('n')).unsafeRunSync())) shouldBe empty
  }
