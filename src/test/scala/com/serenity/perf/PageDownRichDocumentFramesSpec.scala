package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, StatusLinePlacement, TabStripMode}
import com.serenity.keystroke.events.PageDown
import com.serenity.richtext.{InlineMark, ParagraphAlignment, ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun, RichTextStyle, RtfDocumentCodec}
import com.serenity.state.manager.StateManagerTestFacade.updateState
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, Java2DRenderSurface, RendererFramePlanner, ScreenIdentity}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Paging down a chaptered rich document to its end draws, at every step, the frame a cold repaint draws, in both
  * layouts at the device scales a laptop reports. Two images alternate as the window's pool does. At the end of the
  * document the last row is cut off by the pane's bottom edge, which in the conventional layout is the status row.
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

    private val caches = RenderCaches.create()
    private val screen = ScreenIdentity(this)
    private val drawn  = AtomicInteger(0)

    def render(state: AppState, damage: Damage, cursorVisible: Boolean = false): BufferedImage =
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
      val _ = RendererFramePlanner.renderFrame(
        state,
        cursorVisible = cursorVisible,
        surface,
        window.viewport,
        caches.authoritativeScene.forState(state, window.viewport, codeFont, textFont),
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        None,
        Some(FrameOutput(screen, new AtomicReference(None))),
        damage,
        caches = caches
      )
      image

  private def differing(
    window: Window,
    drawn: BufferedImage,
    state: AppState,
    scale: Double,
    cursorVisible: Boolean = false
  ): Seq[(Int, Int)] =
    val cold = Pooled(window, 1, scale, persists = false).render(state, Damage.Everything, cursorVisible)
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
    perFrame: Int = 1,
    blinks: Int = 0
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
        val blinked = (1 to blinks).flatMap { blink =>
          val visible = blink % 2 == 1
          val idle    = differing(window, frames.render(after, Damage.Nothing, visible), after, scale, visible)
          Option.when(idle.nonEmpty)(s"step $step blink $blink top=$top: ${summary(idle)}")
        }.toList
        (
          after,
          blinked.reverse ++ (if diff.isEmpty then Nil else List(s"step $step top=$top: ${summary(diff)}")) ++ found
        )
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
    if (scale == 1.0 || documentName.startsWith("six")) && (layoutName == "minimal" || !documentName.startsWith("Word"))
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
    (shape, perFrame, blinks) <- List(
      ("one PageDown a frame", 1, 0),
      ("three PageDowns a frame", 3, 0),
      ("blinking between PageDowns", 1, 2)
    )
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
          perFrame = perFrame,
          blinks = blinks
        ) shouldBe empty
      }
