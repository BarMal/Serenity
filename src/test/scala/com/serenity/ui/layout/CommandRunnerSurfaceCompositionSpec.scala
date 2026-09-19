package com.serenity.ui.layout

import com.serenity.command.*
import com.serenity.config.{AppConfig, InterfaceConfig, InterfaceDensity}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `CommandRunnerSurfaceComposition` (issue #819, slice 2): the command palette and the settings surface
  * it also hosts, resolved into one paint/hit-test/focus plan -- the same pattern `ModalSurfaceComposition` and
  * `ContextMenuSurfaceComposition` already establish. Content formatting (segments, columns, editing state) is
  * intentionally still sourced from `CommandPaletteContentResolver`'s existing, separately-tested row builders -- this
  * composition owns row *position* and *hit-testing*, not row text.
  *
  * Scope note: this composition only replaces the cell/keyboard-coordinate hit-test path
  * (`MouseHitTestGeometry.overlayDisplayedRowIndexAt`'s row-slot lookup). The sub-cell fractional-pixel hover/click
  * path (`FloatingSurfaceGeometry`, exercised by `CommandRunnerMouseSpec`'s "fractional floating pixel offset" tests)
  * is a distinct, already-tested feature this migration does not touch.
  */
class CommandRunnerSurfaceCompositionSpec extends AnyFlatSpec with Matchers:

  private def commands(n: Int): List[Command] =
    (0 until n)
      .map(i => Command.typed(s"cmd-$i", s"Command $i", CommandIntent.Edit(EditIntent.Copy), label = s"Command $i"))
      .toList

  private def paletteRunner(items: List[Command], selectedIndex: Int = 0): CommandRunner =
    CommandRunner.empty
      .activate(CommandRegistry(items), AppConfig.default)
      .copy(surface =
        CommandRunnerSurface.Palette(CommandPaletteState(filteredCommands = items, selectedIndex = selectedIndex))
      )

  private def settingsRootRunner: CommandRunner =
    CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(surface = CommandRunnerSurface.Settings())

  "forRunner" should "put the live search term in the header for an active palette" in {
    val runner = paletteRunner(commands(3)).copy(surface =
      CommandRunnerSurface.Palette(CommandPaletteState(searchTerm = "co", filteredCommands = commands(3)))
    )
    val resolved =
      CommandRunnerSurfaceComposition.forRunner(
        runner,
        LayoutRect(0, 0, 60, 10),
        itemGapRows = 0.0,
        itemTargetRows = 1,
        showKeyHints = false
      )

    val header = resolved.paintBoxes.headOption.getOrElse(fail("expected a header box"))
    header.text shouldBe Some("search: co")
    header.focusId shouldBe None
  }

  it should "paint one selectable ActionItem box per visible command, addressed by absolute index" in {
    val items  = commands(3)
    val runner = paletteRunner(items, selectedIndex = 1)
    val resolved =
      CommandRunnerSurfaceComposition.forRunner(
        runner,
        LayoutRect(0, 0, 60, 10),
        itemGapRows = 0.0,
        itemTargetRows = 1,
        showKeyHints = false
      )

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

    withItems.paintBoxes.exists(_.text.exists(_.contains("↑↓ move"))) shouldBe true
    empty.paintBoxes.exists(_.text.exists(_.contains("↑↓ move"))) shouldBe false
  }

  it should "add a persistent key-hint row only when showKeyHints is set" in {
    val runner = paletteRunner(commands(2))
    val withHints =
      CommandRunnerSurfaceComposition.forRunner(runner, LayoutRect(0, 0, 60, 10), 0.0, 1, showKeyHints = true)
    val withoutHints =
      CommandRunnerSurfaceComposition.forRunner(runner, LayoutRect(0, 0, 60, 10), 0.0, 1, showKeyHints = false)

    withHints.paintBoxes.exists(_.text.contains("↑↓ move • Enter run • Esc close")) shouldBe true
    withoutHints.paintBoxes.exists(_.text.contains("↑↓ move • Enter run • Esc close")) shouldBe false
  }

  it should "resolve hitAt to the exact command clicked, matching what was painted" in {
    val items = commands(3)
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
    val runner = settingsRootRunner
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

  it should "map an OverlayRowLayout.Distributed row to SurfacePaintLayout.Distributed, preserving its segments faithfully (issue #819 prep)" in {
    val segments = List(
      OverlaySegment("Bold", allocatedWidth = Some(6), trailingSeparator = true),
      OverlaySegment("Italic", allocatedWidth = Some(8))
    )
    val row = OverlayRow(plainText = "Bold Italic", segments = segments, layout = OverlayRowLayout.Distributed)

    val box = CommandRunnerSurfaceComposition.toBox(row, LogicalPixelRect(0, 0, 20, 1), None)

    box.layout shouldBe SurfacePaintLayout.Distributed
    box.segments shouldBe segments
  }

  it should "never make a group's preview rows independently selectable or focusable" in {
    val runner = settingsRootRunner
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

  private def stateForDensity(density: InterfaceDensity): AppState =
    AppState(
      persisted = Persisted(
        layout = Layout.empty,
        buffers = Map.empty,
        focus = Focus.Modal,
        config = AppConfig.default.copy(interfaceConfig = InterfaceConfig(density = density))
      )
    )

  // `FloatingSurfaceLayout.calculateFloatingSurfaceHeight` is `private[layout]`, and this spec shares that package --
  // it is called here directly as the pre-migration reference implementation, so `frameHeight` is proven
  // behavior-preserving against the exact computation `FloatingSurfaceLayout` used to do inline, for every density and
  // room combination, rather than against separately hand-derived numbers that could drift from it.
  //
  // `frameHeight` itself returns the *preferred* height only -- the same "before the shared floor/maxHeight clamp"
  // value `ModalSurfaceComposition.frameHeight` returns for its own content -- because that final
  // `math.max(3, math.min(maxHeight, preferredHeight))` clamp is common to every `SurfaceContent` case and stays in
  // `FloatingSurfaceLayout` itself post-migration, not duplicated into each composition object. Reproducing that one
  // shared clamp here is what makes this comparable to `calculateFloatingSurfaceHeight`'s fully-clamped result.
  private def clamped(maxHeight: Int, preferredHeight: Int): Int =
    math.max(3, math.min(maxHeight, preferredHeight))

  "frameHeight" should "match FloatingSurfaceLayout's existing command-palette height for every density" in {
    val runner = paletteRunner(commands(12))
    for
      density             <- List(InterfaceDensity.Compact, InterfaceDensity.Comfortable, InterfaceDensity.Spacious)
      maxHeight           <- List(6, 12, 24)
      roomOnPreferredSide <- List(4, 12, Int.MaxValue)
    do
      val state   = stateForDensity(density)
      val content = SurfaceContent.CommandPalette(runner)
      val expected =
        FloatingSurfaceLayout.calculateFloatingSurfaceHeight(content, 60, maxHeight, state, roomOnPreferredSide)

      clamped(
        maxHeight,
        CommandRunnerSurfaceComposition.frameHeight(state, maxHeight, roomOnPreferredSide)
      ) shouldBe expected
    end for
  }

  it should "match FloatingSurfaceLayout's existing height for a settings-surface (submenu) runner too" in {
    val runner = settingsRootRunner
    for
      density   <- List(InterfaceDensity.Compact, InterfaceDensity.Comfortable, InterfaceDensity.Spacious)
      maxHeight <- List(6, 12, 24)
    do
      val state    = stateForDensity(density)
      val content  = SurfaceContent.CommandPalette(runner)
      val expected = FloatingSurfaceLayout.calculateFloatingSurfaceHeight(content, 60, maxHeight, state)

      clamped(maxHeight, CommandRunnerSurfaceComposition.frameHeight(state, maxHeight)) shouldBe expected
    end for
  }

  it should "default roomOnPreferredSide to unconstrained, matching FloatingSurfaceLayout's own default" in {
    val state = stateForDensity(InterfaceDensity.Comfortable)

    CommandRunnerSurfaceComposition.frameHeight(state, maxHeight = 20) shouldBe
      CommandRunnerSurfaceComposition.frameHeight(state, maxHeight = 20, roomOnPreferredSide = Int.MaxValue)
  }
