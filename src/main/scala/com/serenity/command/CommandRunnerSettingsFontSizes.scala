package com.serenity.command

/** The "Font Sizes" group at the top of Settings > Typography (#2405): the prose, code and UI sizes together, in every
  * app mode.
  */
private[command] object CommandRunnerSettingsFontSizes:

  val GroupId: String = "settings-font-sizes"

  /** The per-role size rows, and the ids the same rows take in this group: every row in the tree has an id of its own.
    */
  private val AliasOf = Map(
    "text-font-size" -> "font-sizes-prose",
    "code-font-size" -> "font-sizes-code",
    "ui-font-size"   -> "font-sizes-ui"
  )

  private val SetAllId    = "set-all-font-sizes"
  private val GroupRowIds = List("font-sizes-prose", "font-sizes-code", "font-sizes-ui", SetAllId)

  /** The per-role size rows, which settings search leaves out because this group's rows stand for them. */
  def isListedInGroup(id: String): Boolean = AliasOf.contains(id)

  /** `sizeItems` with the rows this group adds: the per-role size rows again under the ids the group lists them with
    * (they edit the same sizes), and Set All Sizes.
    */
  def withGroupRows(sizeItems: List[CommandSurfaceItem.InputItem]): List[CommandSurfaceItem.InputItem] =
    sizeItems ++ sizeItems.flatMap(item => AliasOf.get(item.id).map(alias => item.copy(id = alias))) :+ setAllItem

  def group(
    inputItems: List[CommandSurfaceItem.InputItem],
    context: CommandRunnerContext
  ): CommandSurfaceItem.GroupItem =
    val rows = GroupRowIds.flatMap(id => inputItems.find(_.id == id)).map(shortened)
    val undo = CommandSurfaceItem.CommandItem(
      FontSizeCommands.undo,
      Option.when(context.previousFontSizes.isEmpty)("No size change to undo")
    )
    CommandSurfaceItem.GroupItem(
      id = GroupId,
      label = "Font Sizes",
      children = rows :+ undo,
      category = CommandCategory.Settings,
      hint = Some("Prose, code and UI sizes together")
    )

  /** The hint column of a size row is a few characters wide beside the value and its default, so the group's rows say
    * only what matters there: that the document uses this one, or the unit.
    */
  private def shortened(item: CommandSurfaceItem.InputItem): CommandSurfaceItem.InputItem =
    if AliasOf.values.exists(_ == item.id) then item.copy(hint = "Points")
    else item

  def setAllItem: CommandSurfaceItem.InputItem =
    CommandSurfaceItem.InputItem(
      id = SetAllId,
      label = "Set All Sizes...",
      hint = "8.0-48.0; asks first",
      currentValue = "",
      kind = CommandSurfaceItem.InputKind.Numeric(decimal = true),
      parse = text =>
        text.toFloatOption
          .filter(size => size >= 8.0f && size <= 48.0f)
          .map(size => CommandIntent.Settings(SettingsIntent.Font(FontIntent.ConfirmSetAllFontSizes(size)))),
      category = CommandCategory.Settings
    )
