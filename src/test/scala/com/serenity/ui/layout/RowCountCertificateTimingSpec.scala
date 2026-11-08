package com.serenity.ui.layout

import java.awt.Font
import java.util.concurrent.Executor

import scala.util.Random

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, TypographyRole}
import com.serenity.ui.fonts.FontLoader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A font set with ligatures has its row-count certificate built on another thread the first time it is asked about,
  * and a line is wrapped until it is ready (`RowCertificates`). Two editors measuring the same text, one before and one
  * after the build, must still count the same rows: if they did not, a batch and the same events applied one at a time
  * could leave different viewports and cursor rows depending on when the build finished (#2484, EditRunSettlingSpec on
  * macOS and Windows). Here the build is the executor's inline run or never, so there is no timing to be lucky with.
  */
class RowCountCertificateTimingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val frc              = TextLayoutSnapshot.defaultFontRenderContext()
  private val bufferId         = BufferId(1)
  private val inline: Executor = _.run()
  private val never: Executor  = _ => ()

  private val config  = AppConfig.default
  private val prose   = FontLoader.previewFontForRole(config.editorConfig.fontConfig, TypographyRole.Prose)
  private val code    = FontLoader.previewFontForRole(config.editorConfig.fontConfig, TypographyRole.Code)
  private val ligated = FontLoader.previewCodeFont(FontLoader.FontConfig(fontSize = 12.0f, enableLigatures = true))

  private val words =
    "lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore".split(" ")

  /** The document EditRunSettlingSpec edits: 120 paragraphs of 40 to 119 words. */
  private val proseLines: Vector[String] =
    val random = new Random(1879)
    Vector.fill(120)(Vector.fill(40 + random.nextInt(80))(words(random.nextInt(words.length))).mkString(" ") + ".")

  private val codeLines: Vector[String] = Vector.tabulate(60) { line =>
    if line % 3 == 0 then s"  val f$line = (a: Int) => a -> $line // line $line"
    else if line % 3 == 1 then s"    case Some(x) if x >= $line && x != 1 => x |> g >>= h"
    else s"  def step$line(value: Int): Int = value * 2 + $line"
  }

  private def rowCounts(
    lines: Vector[String],
    widthPx: Int,
    font: Font,
    builder: Executor,
    plantedScale: Option[Float] = None
  ): Vector[Int] =
    val cache = WrappedLineCache.bounded(layoutMemos = LayoutMemos(builder))
    val key   = NominalAdvances.Key(font, frc)
    // The first ask starts the build, which the inline executor finishes before the next ask.
    cache.layoutMemos.rowCertificates.of(key)
    plantedScale.foreach { scale =>
      cache.layoutMemos.rowCertificates.plant(key, cache.layoutMemos.rowCertificates.of(key).get.scaled(scale))
    }
    val rows = VisualRowCounts.forBuffer(
      Buffer.fromString(bufferId, lines.mkString("\n")),
      widthPx,
      font,
      Some(CellMetrics.fromFont(font)),
      false,
      cache
    )
    Vector.tabulate(lines.size)(rows.rowsIn)

  private val widths = List(160, 240, 380, 640, 1024, 1472, 1900)

  List("the prose font" -> prose, "the code font" -> code, "a font set with ligatures" -> ligated).foreach {
    (name, font) =>
      s"The rows of EditRunSettlingSpec's prose in $name" should
        "be the same whether its certificate was built before they were counted or never" in
        widths.foreach { width =>
          withClue(s"$width px: ") {
            rowCounts(proseLines, width, font, inline) shouldBe rowCounts(proseLines, width, font, never)
          }
        }

      s"The rows of source lines in $name" should
        "be the same whether its certificate was built before they were counted or never" in
        widths.foreach { width =>
          withClue(s"$width px: ") {
            rowCounts(codeLines, width, font, inline) shouldBe rowCounts(codeLines, width, font, never)
          }
        }
  }

  "The certificate of the prose font" should "not appear between one ask and the next" in {
    val certificates = LayoutMemos(inline).rowCertificates
    val key          = NominalAdvances.Key(prose, frc)
    val first        = certificates.of(key).isDefined
    val later        = List.fill(3)(certificates.of(key).isDefined)
    withClue(s"first ask $first, later asks $later: ")(later.forall(_ == first) shouldBe true)
  }

  "The certificate of a font set with ligatures" should "be built once asked for, so the comparisons above set a table against a wrap" in {
    val certificates = LayoutMemos(inline).rowCertificates
    val key          = NominalAdvances.Key(ligated, frc)
    certificates.of(key)
    certificates.of(key) shouldBe defined
  }

  "A certificate that undercounts" should "change the rows, so the comparisons above can fail" in {
    // Narrow enough that the source lines wrap, so a certificate that calls them one row is wrong.
    val width = 240
    rowCounts(codeLines, width, ligated, never).exists(_ > 1) shouldBe true
    rowCounts(codeLines, width, ligated, inline, plantedScale = Some(0.05f)) should not be
      rowCounts(codeLines, width, ligated, never)
  }
