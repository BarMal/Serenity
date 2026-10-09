package com.serenity.ui.theme

import cats.effect.unsafe.implicits.global
import com.serenity.ui.color.RenderColor
import com.serenity.ui.theme.config.{ConfigurableThemeManager, ThemeConfigLoader}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ThemeContrastSpec extends AnyFlatSpec with Matchers:

  import ThemeContrastSpec.*

  private val manager = new ConfigurableThemeManager(new ThemeConfigLoader())

  private def bundled(name: String): Theme =
    manager.loadThemeFromResource(s"themes/$name.conf").unsafeRunSync()

  "the bundled high-contrast theme" should "be loadable from the bundled themes" in {
    bundled("high-contrast").name shouldBe "high-contrast"
  }

  it should "meet 7:1 for every text pair and 3:1 for every non-text pair" in {
    val failures = pairsOf(bundled("high-contrast")).filter(pair => pair.ratio < pair.kind.minimum)

    withClue(failures.map(_.describe).mkString("\n", "\n", "\n"))(failures shouldBe empty)
  }

  it should "use a black background and white text" in {
    val theme = bundled("high-contrast")

    theme.background.argb shouldBe 0xff000000
    theme.foreground.argb shouldBe 0xffffffff
  }

  it should "keep the selection apart from both the caret and the page" in {
    val theme = bundled("high-contrast")

    Theme.contrastRatio(theme.cursor, theme.highlighted.background) should be >= NonTextMinimum
    Theme.contrastRatio(theme.highlighted.background, theme.background) should be >= NonTextMinimum
  }

  "the light and dark themes" should "report their contrast pairs, informationally" in {
    List("light", "dark").foreach { name =>
      val pairs = pairsOf(bundled(name))
      val below = pairs.filter(pair => pair.ratio < pair.kind.minimum)
      info(s"$name: ${below.size} of ${pairs.size} pairs below target")
      below.foreach(pair => info(s"$name: ${pair.describe}"))
    }
    succeed
  }

object ThemeContrastSpec:

  val TextMinimum: Double    = 7.0
  val NonTextMinimum: Double = 3.0

  enum PairKind(val minimum: Double):
    case Text    extends PairKind(TextMinimum)
    case NonText extends PairKind(NonTextMinimum)

  final case class ContrastPair(role: String, kind: PairKind, ratio: Double):
    def describe: String = f"$role%-36s $kind%-8s $ratio%.2f:1 (needs ${kind.minimum}%.1f)"

  private def between(role: String, kind: PairKind, foreground: RenderColor, background: RenderColor): ContrastPair =
    ContrastPair(role, kind, Theme.contrastRatio(foreground, background))

  private def within(role: String, kind: PairKind, token: ThemeColor): ContrastPair =
    between(role, kind, token.foreground, token.background)

  private def uiTokenPairs(theme: Theme): List[ContrastPair] =
    List(
      "selection text" -> theme.highlighted,
      "menu item text" -> theme.menuItem,
      "panel text"     -> theme.panel,
      "error text"     -> theme.error,
      "warning text"   -> theme.warning,
      "hover text"     -> theme.interactionStates.hover,
      "pressed text"   -> theme.interactionStates.pressed
    ).map((role, token) => within(role, PairKind.Text, token))

  private def syntaxPairs(theme: Theme): List[ContrastPair] =
    theme.syntaxColors.toList.map { (element, token) =>
      val kind = if element == SyntaxElement.Whitespace then PairKind.NonText else PairKind.Text
      within(s"syntax $element", kind, token)
    }

  private def surfacePairs(theme: Theme): List[ContrastPair] =
    List(
      between("text on background", PairKind.Text, theme.foreground, theme.background),
      between("muted on background", PairKind.Text, theme.muted, theme.background),
      between("muted (gutter) on margin", PairKind.Text, theme.muted, theme.margin),
      between("placeholder on background", PairKind.Text, theme.placeholder, theme.background)
    )

  private def boundaryPairs(theme: Theme): List[ContrastPair] =
    List(
      within("disabled text", PairKind.NonText, theme.interactionStates.disabled),
      between("caret on background", PairKind.NonText, theme.cursor, theme.background),
      between("caret on selection", PairKind.NonText, theme.cursor, theme.highlighted.background),
      between("selection on background", PairKind.NonText, theme.highlighted.background, theme.background),
      between("focus outline on background", PairKind.NonText, theme.focus, theme.background),
      between("focus outline on panel", PairKind.NonText, theme.focus, theme.panel.background),
      between("active pane border on background", PairKind.NonText, theme.activePane, theme.background),
      between("active pane border on panel", PairKind.NonText, theme.activePane, theme.panel.background)
    )

  def pairsOf(theme: Theme): List[ContrastPair] =
    surfacePairs(theme) ++ uiTokenPairs(theme) ++ syntaxPairs(theme) ++ boundaryPairs(theme)
