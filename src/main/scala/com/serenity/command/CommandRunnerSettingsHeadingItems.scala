package com.serenity.command

import com.serenity.config.{HeadingLevelConfig, HeadingsConfig}
import com.serenity.richtext.ParagraphAlignment
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.theme.{HeadingScale, HeadingStyle}

/** The Headings settings group: damping for the whole document, and one sub-group of rows per heading level. Heading
  * settings are global, so none of this is built into a UI preset's page.
  */
private[command] object CommandRunnerSettingsHeadingItems:

  private val Auto = "Auto"

  private val sizePresets: List[Float] = List(1.0f, 1.17f, 1.33f, 1.5f, 1.83f, 2.0f)

  private val spacePresets: List[Float] = List(0.25f, 0.5f, 0.75f, 1.0f, 1.5f, 2.0f)

  /** Space above and below a heading, one row of options and one custom input each. */
  final private case class SpaceRow(
      row: String,
      label: String,
      hint: String,
      change: Option[Float] => HeadingLevelChange,
      current: HeadingLevelConfig => Option[Float]
  )

  private val spaceRows: List[SpaceRow] = List(
    SpaceRow(
      "space-before",
      "Space Before",
      "Room above the heading, in em of its size (whole rows in a terminal)",
      HeadingLevelChange.SpaceBefore(_),
      _.spaceBefore
    ),
    SpaceRow(
      "space-after",
      "Space After",
      "Room below the heading, in em of its size (whole rows in a terminal)",
      HeadingLevelChange.SpaceAfter(_),
      _.spaceAfter
    )
  )

  private val alignments: List[(String, Option[ParagraphAlignment])] = List(
    Auto      -> None,
    "Left"    -> Some(ParagraphAlignment.Left),
    "Centre"  -> Some(ParagraphAlignment.Center),
    "Right"   -> Some(ParagraphAlignment.Right),
    "Justify" -> Some(ParagraphAlignment.Justify)
  )

  private val levels: List[Int] = (1 to HeadingStyle.LevelCount).toList

  private def rowId(level: Int, row: String): String = s"heading-h$level-$row"

  private def levelIntent(level: Int, change: HeadingLevelChange): CommandIntent =
    CommandIntent.Settings(SettingsIntent.Headings(HeadingsIntent.SetLevel(level, change)))

  private def headingsIntent(intent: HeadingsIntent): CommandIntent =
    CommandIntent.Settings(SettingsIntent.Headings(intent))

  /** The current choice of every option row, keyed by row id, as an index into that row's options. */
  def selections(headings: HeadingsConfig): Map[String, Int] =
    Map("headings-damping" -> (if headings.damping then 0 else 1)) ++
      levels.zip(headings.levels).flatMap((level, config) => levelSelections(level, config))

  private def levelSelections(level: Int, config: HeadingLevelConfig): Map[String, Int] =
    def flag(value: Option[Boolean]): Int = value.fold(0)(on => if on then 1 else 2)
    Map(
      rowId(level, "size")   -> config.size.fold(0)(sizeIndex),
      rowId(level, "bold")   -> flag(config.bold),
      rowId(level, "italic") -> flag(config.italic),
      rowId(level, "font")   -> config.family.fold(0)(family => textFamilyIndex(family)),
      rowId(level, "align")  -> alignments.indexWhere(_._2 == config.align)
    ) ++ spaceRows.map(space => rowId(level, space.row) -> space.current(config).fold(0)(spaceIndex)).toMap

  // A size outside the usual multiples is listed after them, as the row's extra option.
  private def sizeIndex(size: Float): Int =
    sizePresets.indexOf(size) match
      case -1    => sizePresets.size + 1
      case index => index + 1

  private def spaceIndex(em: Float): Int =
    spacePresets.indexOf(em) match
      case -1    => spacePresets.size + 1
      case index => index + 1

  private def textFamilyIndex(family: String): Int =
    FontLoader.availableTextFamilies.indexOf(family) + 1

  /** The input rows: each level's custom size and spaces, and the damping threshold. */
  def inputItems(headings: HeadingsConfig): List[CommandSurfaceItem.InputItem] =
    levels.zip(headings.levels).flatMap { (level, config) =>
      customSizeItem(level, config.size) :: spaceRows.map(space => customSpaceItem(level, space, space.current(config)))
    } :+ dampingThresholdItem(headings.dampingThresholdPt)

  private def customSizeItem(level: Int, size: Option[Float]): CommandSurfaceItem.InputItem =
    val (min, max) = HeadingsConfig.SizeRange
    CommandSurfaceItem.InputItem(
      id = rowId(level, "size-custom"),
      label = "Custom Size",
      hint = s"Multiple of the body size ($min-$max), or auto",
      currentValue = size.fold("auto")(_.toString),
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.trim.toLowerCase match
          case "auto" => Some(levelIntent(level, HeadingLevelChange.Size(None)))
          case other =>
            other.toFloatOption
              .filter(value => value >= min && value <= max)
              .map(value => levelIntent(level, HeadingLevelChange.Size(Some(value)))),
      category = CommandCategory.Settings,
      defaultValue = Some("auto")
    )

  private def customSpaceItem(level: Int, space: SpaceRow, em: Option[Float]): CommandSurfaceItem.InputItem =
    val (min, max) = HeadingsConfig.SpaceRange
    CommandSurfaceItem.InputItem(
      id = rowId(level, s"${space.row}-custom"),
      label = s"Custom ${space.label}",
      hint = s"Em of the heading's size ($min-$max), or auto",
      currentValue = em.fold("auto")(_.toString),
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.trim.toLowerCase match
          case "auto" => Some(levelIntent(level, space.change(None)))
          case other =>
            other.toFloatOption
              .filter(value => value >= min && value <= max)
              .map(value => levelIntent(level, space.change(Some(value)))),
      category = CommandCategory.Settings,
      defaultValue = Some("auto")
    )

  private def dampingThresholdItem(threshold: Float): CommandSurfaceItem.InputItem =
    val (min, max) = HeadingsConfig.ThresholdRange
    CommandSurfaceItem.InputItem(
      id = "headings-damping-threshold",
      label = "Damping Threshold",
      hint = s"Body size in points above which heading sizes are damped ($min-$max)",
      currentValue = threshold.toString,
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toFloatOption
          .filter(value => value >= min && value <= max)
          .map(value => headingsIntent(HeadingsIntent.SetDampingThreshold(value))),
      category = CommandCategory.Settings,
      defaultValue = Some(HeadingScale.DefaultDampingThresholdPt.toString)
    )

  def group(
    optionSelections: Map[String, Int],
    inputItems: List[CommandSurfaceItem.InputItem],
    fontFamilies: FontLoader.FontFamilyCatalog,
    onFrontend: SettingsFrontendFilter
  ): CommandSurfaceItem.GroupItem =
    val damping = CommandRunnerSettingsOptionItemHelpers.enabledOptionItem(
      id = "headings-damping",
      label = "Damping",
      selectedIndex = optionSelections.getOrElse("headings-damping", 0),
      enabledIntent = headingsIntent(HeadingsIntent.SetDamping(true)),
      disabledIntent = headingsIntent(HeadingsIntent.SetDamping(false)),
      hint = "Shrink heading sizes toward the body when the body is large"
    )
    CommandSurfaceItem.GroupItem(
      id = "settings-headings",
      label = "Headings",
      children = onFrontend.row(FrontendSupport.GuiOnly, damping).toList ++
        onFrontend.rows(FrontendSupport.GuiOnly, inputItems.filter(_.id == "headings-damping-threshold")) ++
        levels.map(level => levelGroup(level, optionSelections, inputItems, fontFamilies, onFrontend)),
      category = CommandCategory.Settings,
      hint = Some("Size, weight, style, alignment and spacing of each heading level")
    )

  private def levelGroup(
    level: Int,
    optionSelections: Map[String, Int],
    inputItems: List[CommandSurfaceItem.InputItem],
    fontFamilies: FontLoader.FontFamilyCatalog,
    onFrontend: SettingsFrontendFilter
  ): CommandSurfaceItem.GroupItem =
    def selected(row: String): Int = optionSelections.getOrElse(rowId(level, row), 0)
    val guiOnly                    = FrontendSupport.GuiOnly
    val customSize                 = inputItems.find(_.id == rowId(level, "size-custom"))
    val children = List(
      onFrontend.row(guiOnly, sizeItem(level, selected("size"), customSize)),
      customSize.flatMap(onFrontend.row(guiOnly, _)),
      Some(flagItem(level, "bold", "Bold", selected("bold"), HeadingLevelChange.Bold(_))),
      Some(flagItem(level, "italic", "Italic", selected("italic"), HeadingLevelChange.Italic(_))),
      onFrontend.row(guiOnly, fontItem(level, selected("font"), fontFamilies.text)),
      Some(alignItem(level, selected("align")))
    ).flatten ++ spaceRows.flatMap { space =>
      val custom = inputItems.find(_.id == rowId(level, s"${space.row}-custom"))
      Some(spaceItem(level, space, selected(space.row), custom)) ++ custom
    }
    CommandSurfaceItem.GroupItem(
      id = s"settings-heading-h$level",
      label = s"H$level",
      children = children,
      category = CommandCategory.Settings,
      hint = Some(s"Heading level $level")
    )

  private def sizeItem(
    level: Int,
    selectedIndex: Int,
    customSize: Option[CommandSurfaceItem.InputItem]
  ): CommandSurfaceItem.OptionItem =
    val usual = (None :: sizePresets.map(Some(_))).map(scale => scale.fold(Auto)(_.toString) -> scale)
    val custom = customSize
      .flatMap(_.currentValue.toFloatOption)
      .filterNot(sizePresets.contains)
      .map(size => size.toString -> Some(size))
    val options =
      (usual ++ custom).map((label, scale) => CommandOption(label, levelIntent(level, HeadingLevelChange.Size(scale))))
    CommandSurfaceItem.OptionItem(
      id = rowId(level, "size"),
      label = "Size",
      options = options,
      selectedIndex = CommandRunnerSettingsOptionItemHelpers.boundedOptionIndex(selectedIndex, options),
      category = CommandCategory.Settings,
      hint = Some("Multiple of the body size")
    )

  private def spaceItem(
    level: Int,
    space: SpaceRow,
    selectedIndex: Int,
    custom: Option[CommandSurfaceItem.InputItem]
  ): CommandSurfaceItem.OptionItem =
    val usual = (None :: spacePresets.map(Some(_))).map(em => em.fold(Auto)(_.toString) -> em)
    val extra = custom
      .flatMap(_.currentValue.toFloatOption)
      .filterNot(spacePresets.contains)
      .map(em => em.toString -> Some(em))
    val options =
      (usual ++ extra).map((label, em) => CommandOption(label, levelIntent(level, space.change(em))))
    CommandSurfaceItem.OptionItem(
      id = rowId(level, space.row),
      label = space.label,
      options = options,
      selectedIndex = CommandRunnerSettingsOptionItemHelpers.boundedOptionIndex(selectedIndex, options),
      category = CommandCategory.Settings,
      hint = Some(space.hint)
    )

  private def flagItem(
    level: Int,
    row: String,
    label: String,
    selectedIndex: Int,
    change: Option[Boolean] => HeadingLevelChange
  ): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = rowId(level, row),
      label = label,
      options = List(Auto -> None, "On" -> Some(true), "Off" -> Some(false)).map((name, flag) =>
        CommandOption(name, levelIntent(level, change(flag)))
      ),
      selectedIndex = selectedIndex,
      category = CommandCategory.Settings,
      hint = Some(s"$label headings, or the default")
    )

  private def alignItem(level: Int, selectedIndex: Int): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = rowId(level, "align"),
      label = "Alignment",
      options = alignments.map((name, alignment) =>
        CommandOption(name, levelIntent(level, HeadingLevelChange.Align(alignment)))
      ),
      selectedIndex = selectedIndex,
      category = CommandCategory.Settings,
      hint = Some("Alignment of the heading")
    )

  private def fontItem(level: Int, selectedIndex: Int, families: List[String]): CommandSurfaceItem.GroupItem =
    CommandRunnerSettingsItems.fontFamilyGroupItem(
      id = rowId(level, "font"),
      label = "Font",
      selectedIndex = selectedIndex,
      families = Auto :: families,
      intent = family => levelIntent(level, HeadingLevelChange.Family(Option.unless(family == Auto)(family))),
      hint = "Typeface of the heading, or the body's"
    )
