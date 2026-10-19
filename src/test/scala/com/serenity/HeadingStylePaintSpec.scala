package com.serenity

import java.awt.image.BufferedImage

import com.serenity.config.{AppConfig, HeadingsConfig, MarkdownViewMode}
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun}
import com.serenity.state.models.*
import com.serenity.ui.layout.TextRowMetrics
import com.serenity.ui.renderer.EditorPaneRenderPlan
import com.serenity.ui.theme.HeadingSettings
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The graphical editor looks as it always has until a heading level is told otherwise: the terminal's ladder of
  * underline and slant, and its rule, never reach it, while `headings.hN.underline` does.
  */
class HeadingStylePaintSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private val scale = 2.0

  private val levelTexts = List("Alpha", "Bravo", "Charlie", "Delta", "Echo", "Foxtrot")

  private val document = RichTextDocument(
    RichTextParagraph(List(RichTextRun("Opening paragraph of the book.")), role = ParagraphRole.Body) ::
      levelTexts.zipWithIndex.flatMap { (text, index) =>
        List(
          RichTextParagraph(List(RichTextRun(text)), role = ParagraphRole.Heading(index + 1)),
          RichTextParagraph(List(RichTextRun(s"Body under $text.")), role = ParagraphRole.Body)
        )
      }
  )

  private val markdownText =
    levelTexts.zipWithIndex.map((text, index) => s"${"#" * (index + 1)} $text\nBody under $text.").mkString("\n")

  private def richState(config: AppConfig): AppState =
    val base   = stateOf(document.plainText, CursorPosition(0, 0))
    val buffer = base.persisted.buffers(bufferId)
    base.copy(persisted =
      base.persisted.copy(
        buffers = Map(bufferId -> buffer.copy(richText = RichTextState(richTextDocument = Some(document)))),
        config = withMargins(config.withoutStatusLine).withWordWrap(true)
      )
    )

  private def markdownState(config: AppConfig): AppState =
    val base = stateOf(markdownText, CursorPosition(0, 0), language = Some(LanguageId.Markdown))
    base.copy(persisted =
      base.persisted.copy(config = withMargins(config.withoutStatusLine.withSyntaxHighlighting(true)))
    )

  /** Every level set to what it is when left auto in a graphical session; a terminal's ladder would differ from this. */
  private val spelledOut = (1 to 6).foldLeft(HeadingsConfig()) { (headings, level) =>
    headings.withLevel(level)(
      _.copy(bold = Some(true), italic = Some(false), underline = Some(false), rule = Some(true))
    )
  }

  private def configWith(headings: HeadingsConfig): AppConfig = AppConfig.conventional.withHeadingsConfig(headings)

  private def identical(a: BufferedImage, b: BufferedImage): Unit =
    (a.getWidth, a.getHeight) shouldBe (b.getWidth, b.getHeight)
    differingPixels(a, b) shouldBe empty

  "A graphical session's headings" should "resolve to the settings they always have while nothing is set" in {
    AppConfig.conventional.headingSettings shouldBe HeadingSettings.default
  }

  it should "paint rich text exactly as when every level spells out today's look, rule included" in {
    identical(cold(richState(AppConfig.conventional)), cold(richState(configWith(spelledOut))))
  }

  it should "paint Markdown source exactly as when every level spells out today's look" in {
    identical(cold(markdownState(AppConfig.conventional)), cold(markdownState(configWith(spelledOut))))
  }

  it should "paint a live Markdown preview exactly as when every level spells out today's look" in {
    val live = (config: AppConfig) => markdownState(config.withMarkdownViewMode(MarkdownViewMode.LivePreview))

    identical(cold(live(AppConfig.conventional)), cold(live(configWith(spelledOut))))
  }

  it should "ignore a rule, which only a terminal draws" in {
    val ruled = HeadingsConfig().withLevel(1)(_.copy(rule = Some(true))).withLevel(2)(_.copy(rule = Some(true)))

    identical(cold(richState(AppConfig.conventional)), cold(richState(configWith(ruled))))
  }

  private def boxes(plan: EditorPaneRenderPlan): Vector[(TextVisualLine, Int, Int)] =
    val snapshot = plan.snapshots(paneId)
    val content  = plan.paneLayouts(paneId).contentRect
    val metrics  = TextRowMetrics.forSnapshot(content, cellMetrics, snapshot, measured = snapshot.usesMeasuredLayout)
    snapshot.visualLines.zipWithIndex.map((row, index) =>
      (row, metrics.lineTopPx(index), metrics.lineTopPx(index) + metrics.rowHeightPx(index))
    )

  it should "underline a rich-text heading when its level says so, and touch no other row" in {
    val underlined = configWith(HeadingsConfig().withLevel(2)(_.copy(underline = Some(true))))
    val plan       = Session(scale).settle(richState(AppConfig.conventional))
    val before     = cold(richState(AppConfig.conventional))
    val after      = cold(richState(underlined))
    val differing  = differingPixels(before, after)
    val bravo = boxes(plan)
      .find((row, _, _) => row.text == "Bravo")
      .getOrElse(fail("expected the Bravo row"))

    differing should not be empty
    differing.foreach { (_, y) =>
      withClue(s"pixel row $y: ")(
        (y.toDouble / scale >= bravo._2 && y.toDouble / scale <= bravo._3 + 2) shouldBe true
      )
    }
  }

  it should "underline a live Markdown preview heading when its level says so" in {
    val live       = (config: AppConfig) => markdownState(config.withMarkdownViewMode(MarkdownViewMode.LivePreview))
    val underlined = configWith(HeadingsConfig().withLevel(1)(_.copy(underline = Some(true))))

    differingPixels(cold(live(AppConfig.conventional)), cold(live(underlined))) should not be empty
  }
