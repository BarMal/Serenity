package com.serenity.perf

import java.awt.image.BufferedImage
import java.awt.{Font, Rectangle}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, StatusLinePlacement, TabStripMode}
import com.serenity.diagnostics.FrameCounter
import com.serenity.keystroke.events.{Event, InsertChar, MoveDown, MoveUp, PageDown, PageUp, ScrollDown, ScrollUp}
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun}
import com.serenity.setCursorPosition
import com.serenity.state.manager.StateManagerTestFacade.updateState
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.{BundledInterFont, CellMetrics, PixelRect, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, Java2DRenderSurface, RendererFramePlanner, ScreenIdentity}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A drop cap's glyph is one opaque box over several rows, painted by its first row only. Every frame that changes a
  * row under the box must therefore repaint all of them, or the glyph is wiped, or left standing over text that moved.
  * Two images alternate as the window's pool does and the window shows only the rects each frame names, as in
  * [[PageDownRichDocumentFramesSpec]]; the font is pinned so the wraps do not depend on what the platform maps a
  * logical family to.
  */
class DropCapIncrementalFrameSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont    = BundledInterFont.plain(15f)
  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)

  private val windowWidth  = 640
  private val windowHeight = 600
  private val viewport     = ViewportSize(windowWidth / cellMetrics.charWidth, windowHeight / cellMetrics.lineHeight)

  private val config: AppConfig =
    AppConfig.default
      .withStatusLinePlacement(StatusLinePlacement.Pinned)
      .withLineNumbers(false)
      .withPaneHeaders(false)
      .withScrollbar(false)
      .withTabStrip(TabStripMode.Off)

  final private class Pooled(scale: Double, persists: Boolean = true):

    private val pool = 2

    private val images = Vector.fill(if persists then pool else 1)(
      new BufferedImage(
        math.ceil(windowWidth * scale).toInt,
        math.ceil(windowHeight * scale).toInt,
        BufferedImage.TYPE_INT_RGB
      )
    )

    private val screenImage = new BufferedImage(images.head.getWidth, images.head.getHeight, BufferedImage.TYPE_INT_RGB)
    private val caches      = RenderCaches.create()
    caches.frameCounters.setEnabled(true)
    private val screen = ScreenIdentity(this)
    private val drawn  = AtomicInteger(0)

    def render(state: AppState, damage: Damage): BufferedImage =
      val image = images(drawn.getAndIncrement() % images.length)
      val surface = new Java2DRenderSurface(
        image,
        cellMetrics,
        codeFont,
        _ => (),
        logicalWidthPx = windowWidth,
        logicalHeightPx = windowHeight,
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
        viewport,
        caches.authoritativeScene.forState(state, viewport, codeFont, textFont),
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

    def rowsDrawn: Long = caches.frameCounters.lastFrame(FrameCounter.RowsDrawn)

    private def present(image: BufferedImage, region: Option[List[PixelRect]]): Unit =
      val g = screenImage.createGraphics()
      try
        region match
          case None => g.drawImage(image, 0, 0, null)
          case Some(rects) =>
            rects.foreach { rect =>
              val x       = math.floor(rect.xPx * scale).toInt
              val y       = math.floor(rect.yPx * scale).toInt
              val w       = math.ceil(rect.widthPx * scale).toInt
              val h       = math.ceil(rect.heightPx * scale).toInt
              val clipped = new Rectangle(x, y, w, h).intersection(new Rectangle(0, 0, image.getWidth, image.getHeight))
              if !clipped.isEmpty then
                g.drawImage(
                  image.getSubimage(clipped.x, clipped.y, clipped.width, clipped.height),
                  clipped.x,
                  clipped.y,
                  null
                )
            }
      finally g.dispose()

  private def differing(drawn: BufferedImage, state: AppState, scale: Double): Seq[(Int, Int)] =
    val cold = Pooled(scale, persists = false).render(state, Damage.Everything)
    for
      x <- 0 until drawn.getWidth
      y <- 0 until drawn.getHeight
      if (drawn.getRGB(x, y) & 0xffffff) != (cold.getRGB(x, y) & 0xffffff)
    yield (x, y)

  private def summary(d: Seq[(Int, Int)]): String =
    val ys = d.map(_._2)
    s"${d.size} px differ, y ${ys.minOption.getOrElse(0)}-${ys.maxOption.getOrElse(0)}"

  private def words(count: Int, seed: String): String =
    (1 to count).map(n => s"${seed}word$n").mkString(" ")

  private def dropCapParagraph(lines: Int, wordCount: Int, seed: String): RichTextParagraph =
    RichTextParagraph(List(RichTextRun(words(wordCount, seed))), role = ParagraphRole.dropCap(lines))

  private def bodyParagraph(wordCount: Int, seed: String): RichTextParagraph =
    RichTextParagraph(List(RichTextRun(words(wordCount, seed))))

  /** Drop cap paragraphs of `wordCount` words, each followed by two body paragraphs. */
  private def novel(lines: Int, wordCount: Int, chapters: Int = 30): RichTextDocument =
    RichTextDocument(
      (1 to chapters).toList.flatMap { chapter =>
        List(
          dropCapParagraph(lines, wordCount, s"c$chapter"),
          bodyParagraph(70, s"b$chapter"),
          bodyParagraph(30, s"d$chapter")
        )
      }
    )

  final private class Session(document: RichTextDocument, scale: Double):
    private val (stateManager, id) =
      LaptopFrameBenchmarks.stateManagerHolding(document.plainText, config, grid = viewport)
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
    private val frames    = Pooled(scale)
    private val paneId    = stateManager.getCurrentState.unsafeRunSync().persisted.layout.editorPanes.keys.head
    private val shown     = AtomicReference(stateManager.getCurrentState.unsafeRunSync())
    private val failures  = AtomicReference(Vector.empty[String])
    private val stepCount = AtomicInteger(0)
    frames.render(shown.get, Damage.Everything)

    def placeCursor(line: Int, column: Int): Unit =
      val before = shown.get
      stateManager.setCursorPosition(paneId, line, column).unsafeRunSync()
      val after = stateManager.getCurrentState.unsafeRunSync()
      shown.set(after)
      frames.render(after, DamageProducer.forTransition(before, after))

    /** The first edit changes the buffer's chrome as well as its rows; later keystrokes change rows alone. */
    def markModified(): Unit =
      placeCursor(2, 0)
      apply(InsertChar('w'), "first edit")

    def apply(event: Event, label: String): Unit =
      val before = shown.get
      stateManager.applyEvent(event).unsafeRunSync()
      val after = stateManager.getCurrentState.unsafeRunSync()
      shown.set(after)
      val diff = differing(frames.render(after, DamageProducer.forTransition(before, after)), after, scale)
      val n    = stepCount.incrementAndGet()
      if diff.nonEmpty then failures.updateAndGet(_ :+ s"step $n ($label): ${summary(diff)}")

    def mismatches: List[String] = failures.get.take(4).toList

    def rowsDrawnByLastFrame: Long = frames.rowsDrawn

  private val scales = List(1.0, 2.0)

  for
    lines <- List(2, 3, 4, 6)
    scale <- scales
  do
    s"Scrolling past a $lines-line drop cap shorter than its glyph at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(lines, wordCount = 9), scale)
        (1 to 14).foreach(n => session(ScrollDown(1), s"down $n"))
        (1 to 16).foreach(n => session(ScrollUp(1), s"up $n"))
        (1 to 6).foreach(n => session(ScrollDown(3), s"down by three $n"))
        (1 to 6).foreach(n => session(ScrollUp(3), s"up by three $n"))
        (1 to 4).foreach(n => session(PageDown, s"page down $n"))
        (1 to 4).foreach(n => session(PageUp, s"page up $n"))
        session.mismatches shouldBe empty
      }

  for
    lines <- List(2, 3, 4, 6)
    scale <- scales
  do
    s"Scrolling past a $lines-line drop cap that wraps beyond its glyph at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(lines, wordCount = 70), scale)
        (1 to 25).foreach(n => session(ScrollDown(1), s"down $n"))
        (1 to 25).foreach(n => session(ScrollUp(1), s"up $n"))
        (1 to 4).foreach(n => session(PageDown, s"page down $n"))
        (1 to 4).foreach(n => session(PageUp, s"page up $n"))
        session.mismatches shouldBe empty
      }

  for
    lines <- List(2, 3, 4, 6)
    scale <- scales
  do
    s"Typing on the later rows of a $lines-line drop cap paragraph at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(lines, wordCount = 90), scale)
        List(100, 220, 330).foreach { column =>
          session.placeCursor(0, column)
          "ab".foreach(char => session(InsertChar(char), s"type '$char' at column $column"))
        }
        session.mismatches shouldBe empty
      }

  for
    lines <- List(2, 3, 4, 6)
    scale <- scales
  do
    s"Moving the cursor between the rows of a $lines-line drop cap paragraph at ${scale}x" should
      "draw what a cold frame draws at every step" in {
        val session = Session(novel(lines, wordCount = 90), scale)
        session.placeCursor(0, 20)
        (1 to 8).foreach(n => session(MoveDown, s"down $n"))
        (1 to 8).foreach(n => session(MoveUp, s"up $n"))
        session.mismatches shouldBe empty
      }

  private def withoutDropCaps(document: RichTextDocument): RichTextDocument =
    RichTextDocument(document.paragraphs.map(_.copy(role = ParagraphRole.Body)))

  private def rowsDrawnTyping(document: RichTextDocument, line: Int, column: Int): Long =
    val session = Session(document, scale = 1.0)
    session.markModified()
    session.placeCursor(line, column)
    session(InsertChar('x'), s"type at line $line, column $column")
    session.mismatches shouldBe empty
    session.rowsDrawnByLastFrame

  for lines <- List(2, 3, 6) do
    s"Typing in a $lines-line drop cap paragraph" should
      "draw no more than the glyph's rows beyond what the same paragraph without the cap draws" in {
        val capped   = novel(lines, wordCount = 90)
        val uncapped = withoutDropCaps(capped)
        List(20, 100, 220, 330, 500).foreach { column =>
          val extra = rowsDrawnTyping(capped, 0, column) - rowsDrawnTyping(uncapped, 0, column)
          withClue(s"typing at column $column: ")(extra should be <= lines.toLong)
        }
      }

  "Typing in a body paragraph away from every drop cap" should "draw as many rows as with no drop caps at all" in {
    val capped = novel(3, wordCount = 90)

    rowsDrawnTyping(capped, 1, 300) shouldBe rowsDrawnTyping(withoutDropCaps(capped), 1, 300)
  }
