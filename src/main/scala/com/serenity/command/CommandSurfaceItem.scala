package com.serenity.command

sealed trait CommandSurfaceItem:
  def id: String
  def category: CommandCategory
  def searchText: String

object CommandSurfaceItem:

  final case class CommandItem(command: Command, disabledReason: Option[String] = None) extends CommandSurfaceItem:
    override def id: String                = command.name
    override def category: CommandCategory = command.category
    override lazy val searchText: String   = s"${command.name} ${command.label} ${command.description}"

  final case class OptionItem(
      id: String,
      label: String,
      options: List[CommandOption],
      selectedIndex: Int,
      category: CommandCategory,
      hint: Option[String] = None
  ) extends CommandSurfaceItem:
    override lazy val searchText: String =
      s"$label ${options.map(option => s"${option.label} ${option.hint.getOrElse("")}").mkString(" ")}"

    def selectedOption: String =
      options.lift(selectedIndex).map(_.label).getOrElse("")

    def selectedHint: Option[String] =
      options.lift(selectedIndex).flatMap(_.hint).orElse(hint)

    def selectedIntent: Option[CommandIntent] =
      options.lift(selectedIndex).map(_.intent)

    def moveSelection(delta: Int): OptionItem =
      if options.isEmpty then this
      else
        val rawIndex     = (selectedIndex + delta) % options.length
        val wrappedIndex = if rawIndex < 0 then options.length + rawIndex else rawIndex
        copy(selectedIndex = wrappedIndex)

  /** A single independent boolean row -- e.g. a checkbox in a list of toggleable changes -- distinct from
    * [[OptionItem]]'s single choice among N options. `checked` is the item's own baseline value; a runner tracks any
    * in-place flip separately (`CommandRunner.toggleSelections`), the same override-map convention `optionSelections`
    * already uses for `OptionItem.selectedIndex`.
    */
  final case class ToggleItem(
      id: String,
      label: String,
      checked: Boolean,
      category: CommandCategory,
      hint: Option[String] = None
  ) extends CommandSurfaceItem:
    override lazy val searchText: String = s"$label ${hint.getOrElse("")}".trim

    def toggled: ToggleItem = copy(checked = !checked)

  /** What kind of text an [[InputItem]] accepts -- previously encoded as three independent booleans
    * (`isDecimal`/`acceptsBindingText`/`acceptsFreeText`) that were really a single priority-ordered choice, since no
    * call site ever set more than one of them.
    */
  enum InputKind:
    case Numeric(decimal: Boolean)
    case Binding
    case FreeText

  final case class InputItem(
      id: String,
      label: String,
      hint: String,
      currentValue: String,
      kind: InputKind,
      parse: String => Option[CommandIntent],
      category: CommandCategory,
      // issue #1056: the row's default value, formatted exactly like `currentValue` (`None` where no meaningful
      // default exists). Where set, it both renders (`SurfaceContentResolver`) and drives one-key reset: typing the
      // literal "default" resets the row instead of being parsed as a value (`parseOrDefault`).
      defaultValue: Option[String] = None,
      stepFromAuto: Option[Int] = None
  ) extends CommandSurfaceItem:
    override lazy val searchText: String = s"$label $hint"

    def accepts(currentText: String, char: Char): Boolean =
      kind match
        case InputKind.FreeText => !char.isControl
        case InputKind.Binding  => char.isLetterOrDigit || char == '+' || char == '-' || char == '_'
        case InputKind.Numeric(decimal) =>
          char.isDigit || (char == '.' && decimal && !currentText.contains('.')) ||
          // issue #1056: typing "default" resets to the default value (`parseOrDefault`) -- accepted here only
          // while it stays a prefix of that literal word, so ordinary numeric entry is unaffected. Guarded by
          // `defaultValue.nonEmpty` (matching `isOutOfBounds` below) so a field with no default -- e.g.
          // rich-text-font-size -- doesn't accept 'd' as a keystroke only to have it flagged invalid immediately.
          (defaultValue.nonEmpty && InputItem.isDefaultSentinelPrefix(currentText + char))

    def isOutOfBounds(text: String): Boolean =
      // issue #1056: typing "default" (or a prefix of it, mid-keystroke) is never shown as an error -- it is either
      // still becoming the reset sentinel, or, once complete, `parseOrDefault` will accept it as one.
      text.nonEmpty && !(defaultValue.nonEmpty && InputItem.isDefaultSentinelPrefix(text)) && parse(text).isEmpty

    def withCurrentValue(v: String): InputItem = copy(currentValue = v)

    /** Reset-to-default sentinel: typing the word "default" resets the row instead of being parsed as a literal value,
      * the same convention keybinding rows already accept. Falls back to ordinary `parse` when there is no
      * `defaultValue` to reset to (or it doesn't itself parse), so an item with `defaultValue = None` behaves exactly
      * as before -- including a keymap binding row, whose own `parse` already special-cases the literal "default" (and
      * "reset") to reset that binding, entirely independent of this newer `defaultValue` mechanism.
      */
    def parseOrDefault(text: String): Option[CommandIntent] =
      if text.trim.equalsIgnoreCase("default") then defaultValue.flatMap(parse).orElse(parse(text)) else parse(text)

    /** issue #1056: step a numeric setting by one increment without typing, clamped to whatever range this item's own
      * `parse` already enforces. A non-numeric `currentValue` is the Auto sentinel: `+1` steps to `stepFromAuto`, `-1`
      * stays at Auto; non-numeric kinds otherwise return `None`.
      */
    def steppedIntent(direction: Int): Option[CommandIntent] =
      def fromAuto = if direction > 0 then stepFromAuto.flatMap(s => parse(s.toString)) else None
      kind match
        case InputKind.Numeric(decimal) =>
          currentValue.toDoubleOption.fold(fromAuto) { value =>
            val step      = if decimal then InputItem.DecimalStep else InputItem.IntegerStep
            val nextValue = value + (direction * step)
            val nextText  = if decimal then f"$nextValue%.2f" else math.round(nextValue).toString
            parse(nextText)
          }
        case InputKind.Binding | InputKind.FreeText => None

  object InputItem:
    private val DecimalStep = 0.1
    private val IntegerStep = 1.0

    private[command] def isDefaultSentinelPrefix(text: String): Boolean =
      text.nonEmpty && "default".startsWith(text.toLowerCase)

  /** A direct search target for a setting leaf, independent of its rendered label. */
  final case class SettingSearchItem(
      id: String,
      targetGroupId: String,
      targetItemId: String,
      label: String,
      breadcrumb: String,
      effectiveValue: Option[String],
      sourceScope: String,
      category: CommandCategory,
      hint: Option[String] = None
  ) extends CommandSurfaceItem:
    override lazy val searchText: String =
      s"$label ${effectiveValue.getOrElse("")} $sourceScope $breadcrumb ${hint.getOrElse("")}".trim

  final case class GroupItem(
      id: String,
      label: String,
      children: List[CommandSurfaceItem],
      category: CommandCategory,
      hint: Option[String] = None
  ) extends CommandSurfaceItem:
    override lazy val searchText: String =
      s"$label ${children.map(_.searchText).mkString(" ")}"
