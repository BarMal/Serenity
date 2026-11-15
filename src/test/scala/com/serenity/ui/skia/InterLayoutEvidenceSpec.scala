package com.serenity.ui.skia

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import javax.imageio.ImageIO

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{AppState, Buffer, BufferId, EditorPane, Focus, PaneId, Viewport}
import com.serenity.testkit.TestMeasure
import com.serenity.ui.display.DisplayScale.DeviceScale
import com.serenity.ui.layout.{TextLayoutSnapshot, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.renderer.{FakeRenderTarget, FontSpec, RendererEntryPoints}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Evidence of what moving layout from AWT to the Skia measurer does to proportional prose (#2660 item 2, step D2): a
  * fixed 40-line Inter fixture laid out at three pane widths, each row's columns and height and the pane's total height
  * written as a table, and the frame the raster surface draws written as a PNG, under
  * `target/test-reports/layout-evidence/`. Given `-Dlayout.evidence.before=<dir>` holding an earlier run's files, it
  * also writes how many rows moved their wrap point and a PNG of the earlier frame with the pixels that changed in red.
  */
class InterLayoutEvidenceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val Directory = Paths.get("target", "test-reports", "layout-evidence")

  private val WidthsPx = Vector(360, 560, 820)

  private val FrameHeightPx = 900

  private lazy val inter: Font =
    val stream = getClass.getResourceAsStream("/fonts/Inter-Regular-Subset.ttf")
    try Font.createFont(Font.TRUETYPE_FONT, stream).deriveFont(14f)
    finally stream.close()

  private val codeFont = Font.createFont(
    Font.TRUETYPE_FONT,
    getClass.getResourceAsStream("/fonts/CourierPrime-Regular.ttf")
  ).deriveFont(14f)

  private val Sentences = Vector(
    "The quick brown fox jumps over the lazy dog.",
    "Typography affects every Wave and Vase; Tomorrow, after fifteen office coffees, we try again.",
    "AVATAR, Today, Yesterday and Wednesday all kern against their neighbours in a proportional face.",
    "A very long word follows: antidisestablishmentarianism, then more words to fill the rows.",
    "Paragraphs wrap where the measured advances say they must, and nowhere else."
  )

  private val Fixture: String =
    Vector.tabulate(40)(line => Vector.tabulate(1 + line % 3)(part => Sentences((line + part) % 5)).mkString(" "))
      .mkString("\n")

  private def caches(): RenderCaches =
    val owner = RenderCaches.create(measurerFor = backend =>
      TestBundledTypefaces.installInter(backend)
      Some(SkiaTextMeasurer.of(backend))
    )
    val _ = owner.measurer
    owner

  private def buffer: Buffer =
    val base = Buffer.fromString(BufferId(1), Fixture)
    base.copy(viewport = Viewport(visibleLines = 10_000, visibleColumns = 1000))

  final private case class Row(bufferLine: Int, startColumn: Int, endColumn: Int, heightPx: Int)

  private def rowsAt(widthPx: Int, owner: RenderCaches): (Int, Vector[Row]) =
    val snapshot =
      TextLayoutSnapshot.fromBuffer(buffer, widthPx, inter, wrapCache = owner.wrappedLines)
    val rows = snapshot.visualLines.map(line =>
      Row(line.bufferLine, line.startColumn, line.endColumn, if line.heightPx > 0 then line.heightPx else -1)
    )
    (snapshot.lineHeightPx, rows)

  private def frameAt(widthPx: Int, owner: RenderCaches): BufferedImage =
    val cell     = TestMeasure.cellMetrics(codeFont)
    val viewport = ViewportSize(widthPx / cell.charWidth, FrameHeightPx / cell.lineHeight)
    val target   = new FakeRenderTarget(cell, viewport, widthPx, FrameHeightPx, DeviceScale(1.0, 1.0))
    RendererEntryPoints.render(
      stateOf(viewport),
      cursorVisible = false,
      target,
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(inter),
      FontSpec.fromAwt(inter),
      TestMeasure.cellMetrics(inter),
      None,
      repaintOnFlush = true,
      caches = owner,
      surfaces = SkiaRasterSurfaceFactory
    )
    target.published.lastOption.map(_.image).getOrElse(fail("the raster surface published no frame"))

  private def stateOf(viewport: ViewportSize): AppState =
    val paneId  = PaneId(0)
    val sized   = buffer.copy(viewport = Viewport(visibleLines = viewport.height, visibleColumns = viewport.width))
    val initial = AppState.initial(AppConfig.conventional)
    initial.copy(persisted =
      initial.persisted.copy(
        buffers = Map(sized.id -> sized),
        bufferOrder = List(sized.id),
        layout = initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, sized.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        theme = Theme.light,
        config = AppConfig.conventional.withoutStatusLine
      )
    )

  private def table(lineHeightPx: Int, rows: Vector[Row]): String =
    val total = rows.map(row => if row.heightPx > 0 then row.heightPx else lineHeightPx).sum
    s"# lineHeightPx\t$lineHeightPx\n# totalHeightPx\t$total\nrow\tbufferLine\tstart\tend\theightPx\n" +
      rows.zipWithIndex
        .map((row, index) => s"$index\t${row.bufferLine}\t${row.startColumn}\t${row.endColumn}\t${row.heightPx}")
        .mkString("", "\n", "\n")

  private def write(name: String, text: String): Unit =
    val _ = Files.write(Directory.resolve(name), text.getBytes(StandardCharsets.UTF_8))

  private def readRows(file: Path): Vector[(Int, Int, Int)] =
    Files
      .readAllLines(file)
      .toArray(Array.empty[String])
      .toVector
      .filterNot(line => line.startsWith("#") || line.startsWith("row"))
      .map(_.split('\t'))
      .collect { case Array(_, line, start, end, _) => (line.toInt, start.toInt, end.toInt) }

  /** Rows whose wrap point moved: a row of the earlier run that has no row now with the same line and columns. */
  private def movedRows(before: Vector[(Int, Int, Int)], after: Vector[Row]): Int =
    val now = after.map(row => (row.bufferLine, row.startColumn, row.endColumn)).toSet
    before.count(row => !now.contains(row))

  private def overlay(before: BufferedImage, after: BufferedImage): (BufferedImage, Int) =
    val width   = math.max(before.getWidth, after.getWidth)
    val height  = math.max(before.getHeight, after.getHeight)
    val image   = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val changed = for
      y <- 0 until height
      x <- 0 until width
    yield
      def at(frame: BufferedImage) =
        if x < frame.getWidth && y < frame.getHeight then frame.getRGB(x, y) & 0xffffff else 0xffffff
      val differs = at(before) != at(after)
      image.setRGB(x, y, if differs then 0xff0000 else ((at(before) & 0xfefefe) >> 1) + 0x7f7f7f)
      differs
    (image, changed.count(identity))

  "The Inter fixture" should "be laid out and drawn at three pane widths, rows stacking to the pane's height" in {
    val _      = Files.createDirectories(Directory)
    val before = sys.props.get("layout.evidence.before").map(Paths.get(_))
    val summary = WidthsPx.map { widthPx =>
      val owner                = caches()
      val (lineHeightPx, rows) = rowsAt(widthPx, owner)
      write(s"inter-$widthPx.tsv", table(lineHeightPx, rows))
      val frame = frameAt(widthPx, owner)
      ImageIO.write(frame, "png", Directory.resolve(s"inter-$widthPx.png").toFile) shouldBe true

      rows.map(_.bufferLine).distinct shouldBe (0 until 40).toVector
      rows.filter(_.heightPx > 0).map(_.heightPx).distinct.size should be <= 1

      val comparison = before.filter(dir => Files.exists(dir.resolve(s"inter-$widthPx.tsv"))).map { dir =>
        val earlier = readRows(dir.resolve(s"inter-$widthPx.tsv"))
        val earlierFrame = ImageIO.read(dir.resolve(s"inter-$widthPx.png").toFile)
        val (diff, changedPx) = overlay(earlierFrame, frame)
        ImageIO.write(diff, "png", Directory.resolve(s"inter-$widthPx-diff.png").toFile) shouldBe true
        s"\trowsBefore=${earlier.length}\trowsMoved=${movedRows(earlier, rows)}\tchangedPx=$changedPx"
      }
      val total = rows.map(row => if row.heightPx > 0 then row.heightPx else lineHeightPx).sum
      s"$widthPx\tlineHeightPx=$lineHeightPx\trows=${rows.length}\ttotalHeightPx=$total${comparison.getOrElse("")}"
    }
    write("summary.tsv", summary.mkString("", "\n", "\n"))
  }
