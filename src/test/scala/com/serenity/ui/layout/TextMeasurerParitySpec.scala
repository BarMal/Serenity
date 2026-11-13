package com.serenity.ui.layout

import java.awt.Font
import java.nio.file.Paths

import com.serenity.ui.fonts.FontFallback
import com.serenity.ui.layout.TextMeasureCorpus.PinnedFont
import com.serenity.ui.layout.TextMeasureParity.Section
import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.FontSpec
import com.serenity.ui.skia.{SkiaTextMeasurer, TestBundledTypefaces}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Writes where the Skia measurer differs from AWT, and asserts only what holds however large the differences are.
  * Against the checked-in baseline for the bundled fonts (`target/test-reports/text-measure-parity/`). The system fonts
  * have no baseline and AWT is not called for them: their rows are Skia's own absolute values, which only show that the
  * measurer answers on this host and say little about parity.
  */
class TextMeasurerParitySpec extends AnyFlatSpec with Matchers:

  private val faces    = TestBundledTypefaces.cacheWithInter()
  private val measurer = new SkiaTextMeasurer(faces)

  private val systemFonts = Vector(
    PinnedFont("system_monospaced", Font(Font.MONOSPACED, Font.PLAIN, 14), FontFallback.Default),
    PinnedFont("system_sans_serif", Font(Font.SANS_SERIF, Font.PLAIN, 14), FontFallback.Default),
    PinnedFont("system_serif", Font(Font.SERIF, Font.PLAIN, 14), FontFallback.Default)
  )

  private lazy val sections: Vector[Section] =
    val golden              = TextMeasureBaseline.load().fold(reason => fail(reason), identity)
    val skia                = TextMeasureCorpus.fonts.flatMap(pinned => TextMeasureParity.skiaEntries(pinned, measurer))
    val (compared, skipped) = TextMeasureParity.compare(golden, skia)
    Vector(
      Section(
        "Bundled fonts against the checked-in baseline",
        "AWT numbers are `text-measure/baseline.tsv` (captured on CI). Floats in pixels at 14 pt.",
        compared,
        skipped
      )
    )

  private lazy val absolute: Vector[TextMeasureParity.Absolute] =
    Vector(
      TextMeasureParity.Absolute(
        "System fonts: Skia's own values (informational, low value)",
        "No baseline and no AWT call for these, so there is nothing to compare with: it only shows the measurer " +
          "answers on this host. Host dependent.",
        systemFonts.flatMap(pinned => TextMeasureParity.skiaEntries(pinned, measurer))
      )
    )

  "The parity report" should "compare something for every font and write its files" in {
    val directory = Paths.get("target", "test-reports", "text-measure-parity")
    val header = List(
      s"JDK ${System.getProperty("java.version")}, ${System.getProperty("os.name")} ${System.getProperty("os.arch")}",
      "Typeface selection is still reconciled to AWT (`SkiaTypefaces.keyOf`), so a Skia number here is Skia shaping on the typeface AWT's advance picks",
      "The Inter subset is resolved to its own bytes (`TestBundledTypefaces`): it is an AWT-only test font that Skia would not find on a host without Inter installed",
      "Compared: cell metrics, the measured-layout decision, carets of one unwrapped row and one-row line heights",
      "Not compared before layout runs through the measurer (step D): wrap points, row counts, tabs, wide and emoji clusters, right-to-left carets"
    )
    val classified = TextMeasureClassification.classify(sections.flatMap(_.compared), measurer, faces)
    val diagnosis  = TextMeasureCorpus.fonts.flatMap(TextMeasureDiagnosis.lines(_, measurer, faces))
    TextMeasureParity.write(
      directory,
      TextMeasureParity.summary(header, sections, absolute, diagnosis, classified),
      TextMeasureParity.tsv(sections)
    )

    sections.foreach(section => section.compared should not be empty)
    absolute.foreach(table => table.entries should not be empty)
    java.nio.file.Files.exists(directory.resolve("summary.md")) shouldBe true
    java.nio.file.Files.exists(directory.resolve("diffs.tsv")) shouldBe true
  }

  "Skia carets" should "start at zero, never go back and hold a stop per character, wherever they differ from AWT" in {
    for
      pinned <- TextMeasureCorpus.fonts ++ systemFonts
      line   <- TextMeasureCorpus.lines.filterNot(line => TextMeasureParity.notComparable.contains(line.id))
    do
      val spec   = FontSpec.fromAwt(pinned.font)
      val carets = measurer.rawCaretXs(line.text, 0, LineFonts(spec, Vector.empty))
      withClue(s"${pinned.label} ${line.id}: ") {
        carets.length shouldBe line.text.length + 1
        carets(0) shouldBe 0.0f
        carets.toVector.sliding(2).forall(pair => pair.length < 2 || pair(0) <= pair(1)) shouldBe true
      }
  }

  "A Skia shaping" should "hold a cluster map that stays inside the text, for every corpus line (no golden: step A pins none)" in {
    for
      pinned <- TextMeasureCorpus.fonts
      line   <- TextMeasureCorpus.lines
    do
      val spec  = FontSpec.fromAwt(pinned.font)
      val fonts = LineFonts(spec, Vector.empty)
      val runs  = measurer.itemise(line.text, 0, line.text.length, 0, fonts)
      runs.foreach { run =>
        val shaped = measurer.shape(line.text, 0, run, 0.toByte)
        withClue(s"${pinned.label} ${line.id}: ") {
          shaped.clusters.forall(cluster => cluster >= 0 && cluster < run.endColumn - run.startColumn) shouldBe true
          shaped.glyphs.length should be > 0
        }
      }
  }
