package com.serenity

import com.serenity.command.*
import com.serenity.config.{AppConfig, HeadingsConfig}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.richtext.ParagraphAlignment
import com.serenity.ui.fonts.FontLoader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Headings group of the settings palette: its rows, their current selections, the intents they choose, and which
  * of them a terminal shows.
  */
class HeadingSettingsRowsSpec extends AnyFlatSpec with Matchers:

  private val registry = CommandRegistry.default

  private def headings(intent: HeadingsIntent): CommandIntent =
    CommandIntent.Settings(SettingsIntent.Headings(intent))

  private def level(number: Int, change: HeadingLevelChange): CommandIntent =
    headings(HeadingsIntent.SetLevel(number, change))

  private def treeOf(
    config: AppConfig,
    capabilities: FrontendCapabilities = FrontendCapabilities.gui,
    showAll: Boolean = false
  ): List[CommandSurfaceItem.GroupItem] =
    CommandRunner.empty
      .activate(registry, config.withShowAllSettingsRegardlessOfMode(showAll), capabilities)
      .settingsGroups

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  private def find(tree: List[CommandSurfaceItem.GroupItem], id: String): CommandSurfaceItem =
    tree.flatMap(group => group :: descendants(group)).find(_.id == id).getOrElse(fail(s"missing row $id"))

  private def group(tree: List[CommandSurfaceItem.GroupItem], id: String): CommandSurfaceItem.GroupItem =
    find(tree, id) match
      case found: CommandSurfaceItem.GroupItem => found
      case other                               => fail(s"$id is not a group: $other")

  private def option(tree: List[CommandSurfaceItem.GroupItem], id: String): CommandSurfaceItem.OptionItem =
    find(tree, id) match
      case found: CommandSurfaceItem.OptionItem => found
      case other                                => fail(s"$id is not an option row: $other")

  private def input(tree: List[CommandSurfaceItem.GroupItem], id: String): CommandSurfaceItem.InputItem =
    find(tree, id) match
      case found: CommandSurfaceItem.InputItem => found
      case other                               => fail(s"$id is not an input row: $other")

  private def configured(update: HeadingsConfig => HeadingsConfig): AppConfig =
    AppConfig.default.withHeadingsConfig(update(HeadingsConfig()))

  private val allLevels = (1 to 6).toList

  "The Headings group" should "sit beside Typography, with the damping rows and one sub-group per level" in {
    val tree = treeOf(AppConfig.default)
    val ids  = tree.map(_.id)
    val node = group(tree, "settings-headings")

    node.label shouldBe "Headings"
    ids.indexOf("settings-headings") shouldBe ids.indexOf("settings-typography") + 1
    node.children.map(_.id) shouldBe
      List("headings-damping", "headings-damping-threshold") ++ allLevels.map(n => s"settings-heading-h$n")
    allLevels.foreach(n => group(tree, s"settings-heading-h$n").label shouldBe s"H$n")
  }

  it should "give every level a size, custom size, bold, italic, font and alignment row" in {
    val tree = treeOf(AppConfig.default)

    allLevels.foreach { n =>
      group(tree, s"settings-heading-h$n").children.map(_.id) shouldBe
        List("size", "size-custom", "bold", "italic", "font", "align").map(row => s"heading-h$n-$row")
    }
  }

  it should "offer auto and the usual multiples as sizes, each choosing that level's size" in {
    val tree = treeOf(AppConfig.default)

    allLevels.foreach { n =>
      val size = option(tree, s"heading-h$n-size")
      size.options.map(_.label) shouldBe List("Auto", "1.0", "1.17", "1.33", "1.5", "1.83", "2.0")
      size.options.map(_.intent) shouldBe
        List(None, Some(1.0f), Some(1.17f), Some(1.33f), Some(1.5f), Some(1.83f), Some(2.0f))
          .map(scale => level(n, HeadingLevelChange.Size(scale)))
    }
  }

  it should "offer auto, on and off for bold and italic" in {
    val tree = treeOf(AppConfig.default)

    allLevels.foreach { n =>
      val flags = List(None, Some(true), Some(false))
      option(tree, s"heading-h$n-bold").options.map(_.label) shouldBe List("Auto", "On", "Off")
      option(tree, s"heading-h$n-bold").options.map(_.intent) shouldBe
        flags.map(flag => level(n, HeadingLevelChange.Bold(flag)))
      option(tree, s"heading-h$n-italic").options.map(_.intent) shouldBe
        flags.map(flag => level(n, HeadingLevelChange.Italic(flag)))
    }
  }

  it should "offer auto, left, centre, right and justify for alignment" in {
    val align = option(treeOf(AppConfig.default), "heading-h3-align")

    align.options.map(_.label) shouldBe List("Auto", "Left", "Centre", "Right", "Justify")
    align.options.map(_.intent) shouldBe
      List(
        None,
        Some(ParagraphAlignment.Left),
        Some(ParagraphAlignment.Center),
        Some(ParagraphAlignment.Right),
        Some(ParagraphAlignment.Justify)
      ).map(alignment => level(3, HeadingLevelChange.Align(alignment)))
  }

  it should "pick a font from the same families the prose font picker offers, with auto first" in {
    val families = FontLoader.availableTextFamilies
    val font     = group(treeOf(AppConfig.default), "heading-h2-font")

    font.hint shouldBe Some("Auto")
    font.children.size shouldBe families.size + 1
    font.children.map {
      case item: CommandSurfaceItem.CommandItem => item.command.intent
      case other                                => fail(s"unexpected font child $other")
    } shouldBe level(2, HeadingLevelChange.Family(None)) ::
      families.map(family => level(2, HeadingLevelChange.Family(Some(family))))
  }

  it should "start every row on auto, with damping on at 24pt, for the default config" in {
    val tree = treeOf(AppConfig.default)

    allLevels.foreach { n =>
      List("size", "bold", "italic", "align").foreach(row =>
        option(tree, s"heading-h$n-$row").selectedOption shouldBe "Auto"
      )
      group(tree, s"heading-h$n-font").hint shouldBe Some("Auto")
      input(tree, s"heading-h$n-size-custom").currentValue shouldBe "auto"
    }
    option(tree, "headings-damping").selectedOption shouldBe "On"
    input(tree, "headings-damping-threshold").currentValue shouldBe "24.0"
  }

  it should "show the configured value on each row" in {
    val family = FontLoader.availableTextFamilies.lastOption.getOrElse(fail("no text families installed"))
    val tree = treeOf(
      configured(
        _.withLevel(2)(
          _.copy(
            size = Some(1.5f),
            bold = Some(false),
            italic = Some(true),
            family = Some(family),
            align = Some(ParagraphAlignment.Justify)
          )
        ).copy(damping = false, dampingThresholdPt = 40.0f)
      )
    )

    option(tree, "heading-h2-size").selectedOption shouldBe "1.5"
    option(tree, "heading-h2-bold").selectedOption shouldBe "Off"
    option(tree, "heading-h2-italic").selectedOption shouldBe "On"
    option(tree, "heading-h2-align").selectedOption shouldBe "Justify"
    group(tree, "heading-h2-font").hint shouldBe Some(family)
    option(tree, "heading-h1-bold").selectedOption shouldBe "Auto"
    option(tree, "headings-damping").selectedOption shouldBe "Off"
    input(tree, "headings-damping-threshold").currentValue shouldBe "40.0"
  }

  it should "keep a size that is not one of the usual multiples visible and selected" in {
    val tree = treeOf(configured(_.withLevel(4)(_.copy(size = Some(1.25f)))))
    val size = option(tree, "heading-h4-size")

    size.selectedOption shouldBe "1.25"
    size.selectedIntent shouldBe Some(level(4, HeadingLevelChange.Size(Some(1.25f))))
    size.options.size shouldBe 8
    input(tree, "heading-h4-size-custom").currentValue shouldBe "1.25"
  }

  "A custom size" should "parse a multiple of the body, or auto, and reject the rest" in {
    val custom = input(treeOf(AppConfig.default), "heading-h1-size-custom")

    custom.parse("2.5") shouldBe Some(level(1, HeadingLevelChange.Size(Some(2.5f))))
    custom.parse(" 1 ") shouldBe Some(level(1, HeadingLevelChange.Size(Some(1.0f))))
    custom.parse("auto") shouldBe Some(level(1, HeadingLevelChange.Size(None)))
    custom.parse("0.5") shouldBe None
    custom.parse("8.5") shouldBe None
    custom.parse("large") shouldBe None
    custom.defaultValue shouldBe Some("auto")
  }

  "The damping threshold" should "parse points within range and reject the rest" in {
    val threshold = input(treeOf(AppConfig.default), "headings-damping-threshold")

    threshold.parse("40") shouldBe Some(headings(HeadingsIntent.SetDampingThreshold(40.0f)))
    threshold.parse("7") shouldBe None
    threshold.parse("145") shouldBe None
    threshold.parse("wide") shouldBe None
    threshold.defaultValue shouldBe Some("24.0")
  }

  "The damping row" should "switch damping on and off" in {
    option(treeOf(AppConfig.default), "headings-damping").options.map(_.intent) shouldBe
      List(headings(HeadingsIntent.SetDamping(true)), headings(HeadingsIntent.SetDamping(false)))
  }

  "The option selections" should "carry every heading row's current choice" in {
    val selections = CommandRunnerOptionSelections.default(
      configured(
        _.withLevel(5)(_.copy(size = Some(2.0f), bold = Some(true), italic = Some(false))).copy(damping = false)
      )
    )

    selections.get("heading-h5-size") shouldBe Some(6)
    selections.get("heading-h5-bold") shouldBe Some(1)
    selections.get("heading-h5-italic") shouldBe Some(2)
    selections.get("heading-h5-align") shouldBe Some(0)
    selections.get("heading-h5-font") shouldBe Some(0)
    selections.get("headings-damping") shouldBe Some(1)
  }

  "On a terminal the Headings group" should "hide the size and font rows, which a terminal cannot draw" in {
    val tree = treeOf(AppConfig.default, FrontendCapabilities.tui())

    allLevels.foreach { n =>
      group(tree, s"settings-heading-h$n").children.map(_.id) shouldBe
        List("bold", "italic", "align").map(row => s"heading-h$n-$row")
    }
    group(tree, "settings-headings").children.map(_.id).take(2) shouldBe
      List("settings-heading-h1", "settings-heading-h2")
    val ids = tree.flatMap(g => g :: descendants(g)).map(_.id)
    ids should not contain "headings-damping"
    ids should not contain "headings-damping-threshold"
  }

  it should "bring the size and font rows back, led by an inert note, when Show All Settings is on" in {
    val tree = treeOf(AppConfig.default, FrontendCapabilities.tui(), showAll = true)

    group(tree, "settings-heading-h1").children.map(_.id) shouldBe
      List("size", "size-custom", "bold", "italic", "font", "align").map(row => s"heading-h1-$row")
    option(tree, "heading-h1-size").hint.getOrElse("") should startWith("Inert in TUI mode -- ")
    input(tree, "heading-h1-size-custom").hint should startWith("Inert in TUI mode -- ")
    group(tree, "heading-h1-font").hint.getOrElse("") should startWith("Inert in TUI mode -- ")
    option(tree, "heading-h1-bold").hint.getOrElse("") should not include "Inert in TUI mode"
    group(tree, "settings-headings").children.map(_.id).take(2) shouldBe
      List("headings-damping", "headings-damping-threshold")
    option(tree, "headings-damping").hint.getOrElse("") should startWith("Inert in TUI mode -- ")
    input(tree, "headings-damping-threshold").hint should startWith("Inert in TUI mode -- ")
  }

  "A UI preset's settings" should "hold no heading row" in {
    val presets = group(treeOf(AppConfig.default), "settings-ui-presets")

    descendants(presets).map(_.id).filter(_.toLowerCase.contains("heading")) shouldBe Nil
  }
