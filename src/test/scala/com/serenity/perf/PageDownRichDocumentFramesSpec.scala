package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.PageDown
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun, RtfDocumentCodec}
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
  private val logicalW    = 1920
  private val logicalH    = 1080
  private val viewport    = ViewportSize(logicalW / cellMetrics.charWidth, logicalH / cellMetrics.lineHeight)

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

  final private class Pooled(pool: Int, scale: Double, persists: Boolean = true):

    private val images = Vector.fill(pool)(
      new BufferedImage(
        math.ceil(logicalW * scale).toInt,
        math.ceil(logicalH * scale).toInt,
        BufferedImage.TYPE_INT_RGB
      )
    )

    private val caches = RenderCaches.create()
    private val screen = ScreenIdentity(this)
    private val drawn  = AtomicInteger(0)

    def render(state: AppState, damage: Damage): BufferedImage =
      val image = images(drawn.getAndIncrement() % pool)
      val surface = new Java2DRenderSurface(
        image,
        cellMetrics,
        codeFont,
        _ => (),
        logicalWidthPx = logicalW,
        logicalHeightPx = logicalH,
        deviceScaleX = scale,
        deviceScaleY = scale,
        contentPersists = persists,
        layerCacheOwnerOverride = Some(screen)
      )
      val _ = RendererFramePlanner.renderFrame(
        state,
        cursorVisible = false,
        surface,
        viewport,
        caches.authoritativeScene.forState(state, viewport, codeFont, textFont),
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

  private def differing(drawn: BufferedImage, state: AppState, scale: Double): Seq[(Int, Int)] =
    val cold = Pooled(1, scale, persists = false).render(state, Damage.Everything)
    for
      x <- 0 until drawn.getWidth
      y <- 0 until drawn.getHeight
      if (drawn.getRGB(x, y) & 0xffffff) != (cold.getRGB(x, y) & 0xffffff)
    yield (x, y)

  private def summary(d: Seq[(Int, Int)]): String =
    val ys = d.map(_._2)
    s"${d.size} px differ, y ${ys.minOption.getOrElse(0)}-${ys.maxOption.getOrElse(0)}"

  private def pagedThrough(config: AppConfig, pool: Int, scale: Double, document: RichTextDocument): List[String] =
    val (stateManager, id) = LaptopFrameBenchmarks.stateManagerHolding(document.plainText, config, grid = viewport)
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
    val frames = Pooled(pool, scale)
    val first  = stateManager.getCurrentState.unsafeRunSync()
    val _      = frames.render(first, Damage.Everything)
    val steps = (1 to 12).foldLeft((first, List.empty[String])) {
      case ((before, found), step) =>
        stateManager.applyEvent(PageDown).unsafeRunSync()
        val after = stateManager.getCurrentState.unsafeRunSync()
        val diff  = differing(frames.render(after, DamageProducer.forTransition(before, after)), after, scale)
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
    if (scale == 1.0 || documentName.startsWith("six")) && (layoutName == "minimal" || !documentName.startsWith("Word"))
  do
    s"PageDown through $documentName in the $layoutName layout at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        pagedThrough(config, 2, scale, document) shouldBe empty
      }
