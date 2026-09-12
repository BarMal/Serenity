package com.serenity.ui.layout

import com.serenity.command.*
import com.serenity.config.AppConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `CommandRunnerSurfaceComposition` (issue #819, slice 2): the command palette and the settings surface
  * it also hosts, resolved into one paint/hit-test/focus plan -- the same pattern `ModalSurfaceComposition` and
  * `ContextMenuSurfaceComposition` already establish. Content formatting (segments, columns, editing state) is
  * intentionally still sourced from `CommandPaletteContentResolver`'s existing, separately-tested row builders --
  * this composition owns row *position* and *hit-testing*, not row text.
  *
  * Scope note: this composition only replaces the cell/keyboard-coordinate hit-test path
  * (`MouseHitTestGeometry.overlayDisplayedRowIndexAt`'s row-slot lookup). The sub-cell fractional-pixel hover/click
  * path (`FloatingSurfaceGeometry`, exercised by `CommandRunnerMouseSpec`'s "fractional floating pixel offset"
  * tests) is a distinct, already-tested feature this migration does not touch.
  */
class CommandRunnerSurfaceCompositionSpec extends AnyFlatSpec with Matchers:

  private def commands(n: Int): List[Command] =
    (0 until n).map(i =>
      Command.typed(s"cmd-$i", s"Command $i", CommandIntent.Edit(EditIntent.Copy), label = s"Command $i")
    ).toList

  private def paletteRunner(items: List[Command], selectedIndex: Int = 0): CommandRunner =
    CommandRunner.empty
      .activate(CommandRegistry(items), AppConfig.default)
      .copy(surface = CommandRunnerSurface.Palette(CommandPaletteState(filteredCommands = items, selectedIndex = selectedIndex)))

  private def settingsRootRunner: CommandRunner =
    CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(surface = CommandRunnerSurface.Settings())

  "forRunner" should "put the live search term in the header for an active palette" in {
    val runner   = paletteRunner(commands(3)).copy(surface =
      CommandRunnerSurface.Palette(CommandPaletteState(searchTerm = "co", filteredCommands = commands(3)))
    )
    val resolved =
      CommandRunnerSurfaceComposition.forRunner(runner, LayoutRect(0, 0, 60, 10), itemGapRows = 0.0, itemTargetRows = 1, showKeyHints = false)

    val header = resolved.paintBoxes.headOption.getOrElse(fail("expected a header box"))
    header.text shouldBe Some("search: co")
    header.focusId shouldBe None
  }

  it should "paint one selectable ActionItem box per visible command, addressed by absolute index" in {
    val items    = commands(3)
    val runner   = paletteRunner(items, selectedIndex = 1)
    val resolved =
      CommandRunnerSurfaceComposition.forRunner(runner, LayoutRect(0, 0, 60, 10), itemGapRows = 0.0, itemTargetRows = 1, showKeyHints = false)

    val itemBoxes = resolved.paintBoxes.filter(_.focusId.isDefined)
    itemBoxes.map(_.focusId) shouldBe List(
      Some(SurfaceFocusId("command-runner-item-0")),
      Some(SurfaceFocusId("command-runner-item-1")),
      Some(SurfaceFocusId("command-runner-item-2"))
    )
    itemBoxes.map(_.actionId) shouldBe itemBoxes.map(box => box.focusId.map(id => SurfaceActionId(id.value)))
    itemBoxes.map(_.selected) shouldBe List(false, true, false)
  }

  it should "render a dynamic footer only when there are visible items" in {
    val withItems = CommandRunnerSurfaceComposition.forRunner(
      paletteRunner(commands(2)),
      LayoutRect(0, 0, 60, 10),
      itemGapRows = 0.0,
      itemTargetRows = 1,
      showKeyHints = false
    )
    val empty = CommandRunnerSurfaceComposition.forRunner(
      paletteRunner(Nil),
      LayoutRect(0, 0, 60, 10),
      itemGapRows = 0.0,
      itemTargetRows = 1,
      showKeyHints = false
    )

    withItems.paintBoxes.exists(_.text.exists(_.contains("navigate"))) shouldBe true
    empty.paintBoxes.exists(_.text.exists(_.contains("navigate"))) shouldBe false
  }

  it should "add a persistent key-hint row only when showKeyHints is set" in {
    val runner = paletteRunner(commands(2))
    val withHints =
      CommandRunnerSurfaceComposition.forRunner(runner, LayoutRect(0, 0, 60, 10), 0.0, 1, showKeyHints = true)
    val withoutHints =
      CommandRunnerSurfaceComposition.forRunner(runner, LayoutRect(0, 0, 60, 10), 0.0, 1, showKeyHints = false)

    withHints.paintBoxes.exists(_.text.contains("↑↓ navigate • Enter run • Esc dismiss")) shouldBe true
    withoutHints.paintBoxes.exists(_.text.contains("↑↓ navigate • Enter run • Esc dismiss")) shouldBe false
  }

  it should "resolve hitAt to the exact command clicked, matching what was painted" in {
    val items    = commands(3)
    val resolved = CommandRunnerSurfaceComposition.forRunner(
      paletteRunner(items),
      LayoutRect(0, 0, 60, 10),
      itemGapRows = 0.0,
      itemTargetRows = 1,
      showKeyHints = false
    )

    val itemBox = resolved.paintBoxes.find(_.text.exists(_.contains("Command 2"))).get
    val hit     = resolved.hitAt(itemBox.rect.x, itemBox.rect.y)

    hit.map(_.focusId) shouldBe Some(SurfaceFocusId("command-runner-item-2"))
    hit.flatMap(_.actionId) shouldBe Some(SurfaceActionId("command-runner-item-2"))
  }

  it should "resolve every settings-root item box back to itself and to the correct underlying group, including any group whose preview rows sit before it" in {
    val runner   = settingsRootRunner
    val resolved = CommandRunnerSurfaceComposition.forRunner(
      runner,
      LayoutRect(0, 0, 90, 24),
      itemGapRows = 0.0,
      itemTargetRows = 1,
      showKeyHints = false
    )
    val items = runner.settingsSurfaceItems

    val itemBoxes = resolved.paintBoxes.filter(_.focusId.isDefined)
    itemBoxes should not be empty

    itemBoxes.foreach { box =>
      val hit = resolved.hitAt(box.rect.x, box.rect.y).getOrElse(fail(s"expected a hit at ${box.rect}"))
      hit.focusId shouldBe box.focusId.get

      val absoluteIndex = box.focusId.get.value.stripPrefix("command-runner-item-").toInt
      val underlying    = items.lift(absoluteIndex).getOrElse(fail(s"no settings item at index $absoluteIndex"))
      box.text shouldBe defined
      box.text.get should include(underlying.searchText.split(" ").headOption.getOrElse(underlying.id))
    }
  }

  it should "never make a group's preview rows independently selectable or focusable" in {
    val runner   = settingsRootRunner
    val resolved = CommandRunnerSurfaceComposition.forRunner(
      runner,
      LayoutRect(0, 0, 90, 12),
      itemGapRows = 0.0,
      itemTargetRows = 1,
      showKeyHints = false
    )

    val previewBoxes = resolved.paintBoxes.filter(box => box.text.exists(_.startsWith("  ")))
    previewBoxes should not be empty
    previewBoxes.foreach { box =>
      box.focusId shouldBe None
      box.actionId shouldBe None
    }
    resolved.focusOrder.length shouldBe resolved.paintBoxes.count(_.focusId.isDefined)
  }
