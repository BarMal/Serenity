package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, AppMode, InterfaceDensity, ProseMeasure, StatusLinePlacement, TabStripMode}
import com.serenity.keystroke.events.PageDown
import com.serenity.richtext.{
  InlineMark,
  ParagraphAlignment,
  ParagraphRole,
  RichTextDocument,
  RichTextParagraph,
  RichTextRun,
  RichTextStyle,
  RtfDocumentCodec
}
import com.serenity.state.manager.StateManagerTestFacade.updateState
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, PixelRect, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, Java2DRenderSurface, RendererFramePlanner, ScreenIdentity}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Paging down a chaptered rich document to its end shows, at every step, the frame a cold repaint draws, in both
  * layouts at the device scales a laptop reports. Two images alternate as the window's pool does, and the window shows
  * only the rects each frame names for repainting, so a band the frame clears but never publishes stays stale on
  * screen, as it did below the last row once a scroll left the pane with fewer rows.
  */
class PageDownRichDocumentFramesSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont    = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)

  final private case class Window(width: Int, height: Int):
    def viewport: ViewportSize = ViewportSize(width / cellMetrics.charWidth, height / cellMetrics.lineHeight)

  private val fullHd = Window(1920, 1080)

  private def chapters(count: Int, bodies: Int): RichTextDocument =
    RichTextDocument(
      (1 to count).toList.flatMap { chapter =>
        RichTextParagraph(List(RichTextRun(s"Chapter $chapter")), role = ParagraphRole.Heading(1)) ::
          (1 to bodies).toList.map { body =>
            RichTextParagraph(
              List(
                RichTextRun(s"Chapter $chapter paragraph $body. " + "lorem ipsum dolor sit amet " * (6 + body * 7 % 23))
              )
            )
          }
      }
    )

  final private class Pooled(window: Window, pool: Int, scale: Double, persists: Boolean = true):

    private val images = Vector.fill(pool)(
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
      val image = images(drawn.getAndIncrement() % pool)
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

    /** What the window shows after the frame is published: only the rects the frame names change on screen. */
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

  private def differing(
    window: Window,
    drawn: BufferedImage,
    state: AppState,
    scale: Double
  ): Seq[(Int, Int)] =
    val cold = Pooled(window, 1, scale, persists = false).render(state, Damage.Everything)
    for
      x <- 0 until drawn.getWidth
      y <- 0 until drawn.getHeight
      if (drawn.getRGB(x, y) & 0xffffff) != (cold.getRGB(x, y) & 0xffffff)
    yield (x, y)

  private def summary(d: Seq[(Int, Int)]): String =
    val ys = d.map(_._2)
    s"${d.size} px differ, y ${ys.minOption.getOrElse(0)}-${ys.maxOption.getOrElse(0)}"

  private def pagedThrough(
    config: AppConfig,
    window: Window,
    pool: Int,
    scale: Double,
    document: RichTextDocument,
    pageDowns: Int = 12,
    perFrame: Int = 1
  ): List[String] =
    val (stateManager, id) =
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
    val frames = Pooled(window, pool, scale)
    val first  = stateManager.getCurrentState.unsafeRunSync()
    val _      = frames.render(first, Damage.Everything)
    val steps = (1 to pageDowns).foldLeft((first, List.empty[String])) {
      case ((before, found), step) =>
        (1 to perFrame).foreach(_ => stateManager.applyEvent(PageDown).unsafeRunSync())
        val after = stateManager.getCurrentState.unsafeRunSync()
        val diff  = differing(window, frames.render(after, DamageProducer.forTransition(before, after)), after, scale)
        val top   = after.persisted.buffers.get(id).map(_.viewport.topLine)
        (after, if diff.isEmpty then found else s"step $step top=$top: ${summary(diff)}" :: found)
    }
    steps._2.reverse.take(3)

  private val wordSample: RichTextDocument =
    RtfDocumentCodec
      .readBytes(Files.readAllBytes(Path.of("src/test/resources/richtext/word-sample.rtf")))
      .getOrElse(fail("expected word-sample.rtf to decode"))

  private val documents = List(
    "six chapters of twelve paragraphs" -> chapters(6, 12),
    "eight chapters of four paragraphs" -> chapters(8, 4),
    "a short last page"                 -> RichTextDocument(chapters(5, 3).paragraphs.take(18)),
    "Word's sample"                     -> wordSample
  )

  for
    (layoutName, config)     <- List("minimal" -> AppConfig.default, "conventional" -> AppConfig.conventional)
    scale                    <- List(1.0, 2.0)
    (documentName, document) <- documents
    if scale == 1.0 || documentName.startsWith("six") || documentName.startsWith("Word")
  do
    s"PageDown through $documentName in the $layoutName layout at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        pagedThrough(config, fullHd, 2, scale, document) shouldBe empty
      }

  private val ownersConfig: AppConfig =
    AppConfig.default
      .withStatusLinePlacement(StatusLinePlacement.Pinned)
      .withLineNumbers(false)
      .withPaneHeaders(false)
      .withScrollbar(false)
      .withTabStrip(TabStripMode.Off)

  private val windows = List(
    "1920x1080"                 -> fullHd,
    "800x600"                   -> Window(800, 600),
    "a height off the row grid" -> Window(1000, 777),
    "a short, wide window"      -> Window(1400, 333)
  )

  private def novel(count: Int, bodies: Int): RichTextDocument =
    val big = RichTextStyle(fontSize = Some(28f), marks = Set(InlineMark.Bold))
    RichTextDocument(
      (1 to count).toList.flatMap { chapter =>
        val bodyParagraphs = (1 to bodies).toList.map { body =>
          val text =
            s"Chapter $chapter paragraph $body. " + "the mage said nothing and watched the rain " * (4 + body * 5 % 19)
          RichTextParagraph(
            List(RichTextRun(text.take(40)), RichTextRun(text.drop(40), RichTextStyle(marks = Set(InlineMark.Italic)))),
            alignment = ParagraphAlignment.Justify
          )
        }
        List(
          RichTextParagraph(
            List(RichTextRun(s"Chapter $chapter", big)),
            ParagraphAlignment.Center,
            ParagraphRole.Heading(1)
          ),
          RichTextParagraph(List(RichTextRun("")), ParagraphAlignment.Center),
          RichTextParagraph(List(RichTextRun("A subtitle")), ParagraphAlignment.Center, ParagraphRole.Heading(2))
        ) ++ bodyParagraphs.headOption.map(_.copy(role = ParagraphRole.dropCap())).toList ++ bodyParagraphs.drop(1)
      }
    )

  private val chaptered = List(
    "centred headings over justified, italic prose" -> novel(6, 10),
    "chapters ending in a short one" -> RichTextDocument(
      chapters(4, 9).paragraphs ++ chapters(1, 2).paragraphs.map(p =>
        p.copy(runs = p.runs.map(r => r.copy(text = r.text.replace("Chapter 1", "Chapter 5"))))
      )
    ),
    "six chapters of twelve paragraphs" -> chapters(6, 12),
    "a single long chapter"             -> chapters(1, 40)
  )

  for
    (chromeName, config) <- List(
      "the owner's chrome (status pinned, no scrollbar)" -> ownersConfig,
      "pinned status with a scrollbar"                   -> ownersConfig.withScrollbar(true)
    )
    scale                    <- List(1.0, 2.0)
    (windowName, window)     <- windows
    (documentName, document) <- chaptered
  do
    s"PageDown through $documentName with $chromeName at ${scale}x in a $windowName window" should
      "draw what a cold frame draws at every step, past the end too" in {
        pagedThrough(config, window, 2, scale, document, pageDowns = 40) shouldBe empty
      }

  for
    (shape, perFrame)        <- List(("one PageDown a frame", 1), ("three PageDowns a frame", 3))
    scale                    <- List(1.0, 2.0)
    (documentName, document) <- ("Word's sample" -> wordSample) :: chaptered.take(2)
  do
    s"PageDown through $documentName with the owner's chrome at ${scale}x with $shape" should
      "draw what a cold frame draws at every step" in {
        pagedThrough(
          ownersConfig,
          fullHd,
          2,
          scale,
          document,
          pageDowns = 20,
          perFrame = perFrame
        ) shouldBe empty
      }

  for
    scale <- List(1.0, 2.0)
    (windowName, window) <- List(
      "1500x1000" -> Window(1500, 1000),
      "1920x1080" -> fullHd,
      "800x600"   -> Window(800, 600)
    )
    (documentName, document) <- List(
      "two short chapters"    -> novel(2, 4),
      "chapters five and six" -> RichTextDocument(novel(6, 3).paragraphs.drop(0).takeRight(2 * (3 + 3 + 0)))
    )
  do
    s"PageDown through $documentName with the focused text body dimmed at ${scale}x in a $windowName window" should
      "draw what a cold frame draws at every step" in {
        pagedThrough(ownersConfig.withFocusedTextBody(true), window, 2, scale, document, pageDowns = 6) shouldBe empty
      }

  private val writingConfig: AppConfig =
    ownersConfig
      .withAppMode(AppMode.Prose)
      .withTypewriterScrolling(true)
      .withFocusedTextBody(true)
      .withProseMeasure(Some(ProseMeasure.Default))
      .withInterfaceDensity(InterfaceDensity.Spacious)

  for
    scale <- List(1.0, 2.0)
    (windowName, window) <- List(
      "1500x1000" -> Window(1500, 1000),
      "1920x1080" -> fullHd,
      "800x600"   -> Window(800, 600)
    )
    (documentName, document) <- List(
      "two short chapters" -> novel(2, 4),
      "six chapters"       -> novel(6, 6),
      "plain chapters"     -> chapters(3, 3)
    )
  do
    s"PageDown through $documentName with typewriter scrolling and dimming at ${scale}x in a $windowName window" should
      "draw what a cold frame draws at every step" in {
        pagedThrough(writingConfig, window, 2, scale, document, pageDowns = 8) shouldBe empty
      }

  private def endingInAnEmptyChapter(chapterCount: Int, bodies: Int): RichTextDocument =
    RichTextDocument(
      chapters(chapterCount, bodies).paragraphs ++ List(
        RichTextParagraph(List(RichTextRun(s"Chapter ${chapterCount + 1}")), role = ParagraphRole.Heading(1)),
        RichTextParagraph(List(RichTextRun("")))
      )
    )

  for
    (configName, config) <- List(
      "the owner's chrome with dimming" -> ownersConfig.withFocusedTextBody(true),
      "typewriter scrolling"            -> writingConfig
    )
    scale                <- List(1.0, 2.0)
    (windowName, window) <- List("1500x1000" -> Window(1500, 1000), "800x600" -> Window(800, 600))
    (documentName, document) <- List(
      "an empty last chapter"      -> endingInAnEmptyChapter(5, 6),
      "a short empty last chapter" -> endingInAnEmptyChapter(2, 3)
    )
  do
    s"PageDown through $documentName with $configName at ${scale}x in a $windowName window" should
      "draw what a cold frame draws, with nothing left on the rows past the end" in {
        pagedThrough(config, window, 2, scale, document, pageDowns = 12) shouldBe empty
      }

  "A sweep of chapter shapes ending in an empty chapter" should "draw what a cold frame draws" in {
    val failures = for
      (configName, config) <- List("dimming" -> ownersConfig.withFocusedTextBody(true), "typewriter" -> writingConfig)
      window               <- List(Window(1500, 1000), Window(800, 600))
      count                <- 2 to 5
      bodies               <- 1 to 7
      found                <- pagedThrough(config, window, 2, 1.0, endingInAnEmptyChapter(count, bodies), pageDowns = 5)
    yield s"$configName $window chapters=$count bodies=$bodies: $found"
    failures shouldBe empty
  }
