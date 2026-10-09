package com.serenity.config

import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.given
import io.circe.{Decoder, Encoder}
import org.slf4j.LoggerFactory

/** What a global hotkey is for -- the section of Settings › Keys › Global it is listed under. */
enum HotkeyPurpose(val label: String):
  case Navigation extends HotkeyPurpose("Navigation")
  case Files      extends HotkeyPurpose("Files")
  case Editing    extends HotkeyPurpose("Editing")
  case View       extends HotkeyPurpose("View")

enum HotkeyAction:
  case Save
  case Quit
  case Undo
  case Redo
  case Copy
  case Paste
  case Cut
  case SelectAll
  case ToggleSyntaxHighlighting
  case OpenFile
  case ToggleCommandRunner
  case ToggleContextualToolbar
  case NewTab
  case CloseTab
  case SplitPaneHorizontal
  case SplitPaneVertical
  case ClosePane
  case FileSearch
  case GoToFile
  case NextTab
  case PreviousTab
  case MoveTabLeft
  case MoveTabRight
  case Find
  case Replace
  case GoToLine
  case SaveAs
  case ToggleShortcutsHelp
  case FocusLeft
  case FocusRight
  case FocusUp
  case FocusDown
  case ToggleChapterGhosts
  case OpenChapterNote
  case ToggleNotesPin

  def configKey: String =
    this match
      case Save                     => "save"
      case Quit                     => "quit"
      case Undo                     => "undo"
      case Redo                     => "redo"
      case Copy                     => "copy"
      case Paste                    => "paste"
      case Cut                      => "cut"
      case SelectAll                => "select_all"
      case ToggleSyntaxHighlighting => "toggle_syntax_highlighting"
      case OpenFile                 => "open_file"
      case ToggleCommandRunner      => "command_palette"
      case ToggleContextualToolbar  => "contextual_toolbar"
      case NewTab                   => "new_tab"
      case CloseTab                 => "close_tab"
      case SplitPaneHorizontal      => "split_pane_horizontal"
      case SplitPaneVertical        => "split_pane_vertical"
      case ClosePane                => "close_pane"
      case FileSearch               => "file_search"
      case GoToFile                 => "go_to_file"
      case NextTab                  => "next_tab"
      case PreviousTab              => "previous_tab"
      case MoveTabLeft              => "move_tab_left"
      case MoveTabRight             => "move_tab_right"
      case Find                     => "find"
      case Replace                  => "replace"
      case GoToLine                 => "go_to_line"
      case SaveAs                   => "save_as"
      case ToggleShortcutsHelp      => "toggle_shortcuts_help"
      case FocusLeft                => "focus_left"
      case FocusRight               => "focus_right"
      case FocusUp                  => "focus_up"
      case FocusDown                => "focus_down"
      case ToggleChapterGhosts      => "toggle_chapter_ghosts"
      case OpenChapterNote          => "open_chapter_note"
      case ToggleNotesPin           => "toggle_notes_pin"

  def purpose: HotkeyPurpose =
    this match
      case Save | SaveAs | OpenFile | NewTab | CloseTab | Quit                      => HotkeyPurpose.Files
      case Undo | Redo | Copy | Paste | Cut | SelectAll | Find | Replace | GoToLine => HotkeyPurpose.Editing
      case ToggleCommandRunner | FileSearch | GoToFile | NextTab | PreviousTab | MoveTabLeft | MoveTabRight |
          SplitPaneHorizontal | SplitPaneVertical | ClosePane | FocusLeft | FocusRight | FocusUp | FocusDown =>
        HotkeyPurpose.Navigation
      case ToggleShortcutsHelp | ToggleContextualToolbar | ToggleSyntaxHighlighting | ToggleChapterGhosts |
          OpenChapterNote | ToggleNotesPin =>
        HotkeyPurpose.View

final case class HotkeyTrigger(
    keyType: InputKey,
    character: Option[Char],
    modifiers: Set[Modifier]
):

  def matches(info: KeyStrokeInfo): Boolean =
    info.keyType == keyType &&
      info.character == character &&
      info.modifiers == modifiers

  /** True for a double-tap-a-lone-modifier binding (`ctrl+ctrl`, `meta+meta`, ...) -- `keyType` itself is the modifier
    * key, not a regular key held down with modifiers. No CSI-u tier below
    * [[com.serenity.keystroke.KeyboardFidelityTier.Full]] can represent a bare modifier press/release event, so this
    * predicate is what `CommandRunnerReducer`'s tier-fidelity warning keys off.
    */
  def isBareModifierChord: Boolean =
    Set(InputKey.Ctrl, InputKey.Alt, InputKey.Shift, InputKey.Meta).contains(keyType)

  def render: String =
    val modifierParts =
      List(
        Option.when(modifiers.contains(Modifier.Ctrl))("ctrl"),
        Option.when(modifiers.contains(Modifier.Alt))("alt"),
        Option.when(modifiers.contains(Modifier.Meta))("meta"),
        Option.when(modifiers.contains(Modifier.Shift))("shift")
      ).flatten
    val keyPart =
      keyType match
        case InputKey.Character  => character.map(_.toString).getOrElse("")
        case InputKey.Ctrl       => "ctrl"
        case InputKey.Alt        => "alt"
        case InputKey.Shift      => "shift"
        case InputKey.Meta       => "meta"
        case InputKey.Tab        => "tab"
        case InputKey.ReverseTab => "reverse-tab"
        case InputKey.Enter      => "enter"
        case InputKey.Backspace  => "backspace"
        case InputKey.Delete     => "delete"
        case InputKey.Escape     => "escape"
        case InputKey.ArrowUp    => "up"
        case InputKey.ArrowDown  => "down"
        case InputKey.ArrowLeft  => "left"
        case InputKey.ArrowRight => "right"
        case InputKey.Home       => "home"
        case InputKey.End        => "end"
        case InputKey.PageUp     => "pageup"
        case InputKey.PageDown   => "pagedown"
        case InputKey.F1         => "f1"
        case InputKey.F2         => "f2"
        case InputKey.F3         => "f3"
        case InputKey.F4         => "f4"
        case InputKey.F5         => "f5"
        case InputKey.F6         => "f6"
        case InputKey.F7         => "f7"
        case InputKey.F8         => "f8"
        case InputKey.F9         => "f9"
        case InputKey.F10        => "f10"
        case InputKey.F11        => "f11"
        case InputKey.F12        => "f12"
        case InputKey.EOF        => "eof"
        case other               => other.toString.toLowerCase
    if Set(InputKey.Ctrl, InputKey.Alt, InputKey.Shift, InputKey.Meta).contains(keyType) then s"$keyPart+$keyPart"
    else (modifierParts :+ keyPart).mkString("+")

object HotkeyTrigger:

  def parse(input: String): Option[HotkeyTrigger] =
    val parts = input.trim.toLowerCase.split("\\+").toList.map(_.trim).filter(_.nonEmpty)
    parts match
      case key :: second :: Nil if key == second =>
        modifierKey(key)
          .map(inputKey => HotkeyTrigger(inputKey, None, Set.empty))
          .orElse(parseStandard(parts))
      case _ =>
        parseStandard(parts)

  private def parseStandard(parts: List[String]): Option[HotkeyTrigger] =
    val (modifierParts, keyParts) = parts.partition {
      case "ctrl" | "alt" | "shift" | "meta" | "cmd" | "command" => true
      case _                                                     => false
    }

    val modifiers = modifierParts.foldLeft(Set.empty[Modifier]) {
      case (acc, "ctrl")                     => acc + Modifier.Ctrl
      case (acc, "alt")                      => acc + Modifier.Alt
      case (acc, "shift")                    => acc + Modifier.Shift
      case (acc, "meta" | "cmd" | "command") => acc + Modifier.Meta
      case (acc, _)                          => acc
    }

    keyParts match
      case key :: Nil =>
        key match
          case "enter" =>
            Some(HotkeyTrigger(InputKey.Enter, None, modifiers))
          case "backspace" =>
            Some(HotkeyTrigger(InputKey.Backspace, None, modifiers))
          case "delete" =>
            Some(HotkeyTrigger(InputKey.Delete, None, modifiers))
          case "escape" =>
            Some(HotkeyTrigger(InputKey.Escape, None, modifiers))
          case "tab" =>
            Some(HotkeyTrigger(InputKey.Tab, None, modifiers))
          case "reverse-tab" | "reverse_tab" =>
            Some(HotkeyTrigger(InputKey.ReverseTab, None, modifiers))
          case "up" | "arrowup" =>
            Some(HotkeyTrigger(InputKey.ArrowUp, None, modifiers))
          case "down" | "arrowdown" =>
            Some(HotkeyTrigger(InputKey.ArrowDown, None, modifiers))
          case "left" | "arrowleft" =>
            Some(HotkeyTrigger(InputKey.ArrowLeft, None, modifiers))
          case "right" | "arrowright" =>
            Some(HotkeyTrigger(InputKey.ArrowRight, None, modifiers))
          case "home" =>
            Some(HotkeyTrigger(InputKey.Home, None, modifiers))
          case "end" =>
            Some(HotkeyTrigger(InputKey.End, None, modifiers))
          case "pageup" | "page_up" =>
            Some(HotkeyTrigger(InputKey.PageUp, None, modifiers))
          case "pagedown" | "page_down" =>
            Some(HotkeyTrigger(InputKey.PageDown, None, modifiers))
          case "f1" =>
            Some(HotkeyTrigger(InputKey.F1, None, modifiers))
          case "f2" =>
            Some(HotkeyTrigger(InputKey.F2, None, modifiers))
          case "f3" =>
            Some(HotkeyTrigger(InputKey.F3, None, modifiers))
          case "f4" =>
            Some(HotkeyTrigger(InputKey.F4, None, modifiers))
          case "f5" =>
            Some(HotkeyTrigger(InputKey.F5, None, modifiers))
          case "f6" =>
            Some(HotkeyTrigger(InputKey.F6, None, modifiers))
          case "f7" =>
            Some(HotkeyTrigger(InputKey.F7, None, modifiers))
          case "f8" =>
            Some(HotkeyTrigger(InputKey.F8, None, modifiers))
          case "f9" =>
            Some(HotkeyTrigger(InputKey.F9, None, modifiers))
          case "f10" =>
            Some(HotkeyTrigger(InputKey.F10, None, modifiers))
          case "f11" =>
            Some(HotkeyTrigger(InputKey.F11, None, modifiers))
          case "f12" =>
            Some(HotkeyTrigger(InputKey.F12, None, modifiers))
          case "eof" =>
            Some(HotkeyTrigger(InputKey.EOF, None, modifiers))
          case single if single.length == 1 =>
            Some(HotkeyTrigger(InputKey.Character, Some(single.head), modifiers))
          case _ =>
            None
      case _ =>
        None

  private def modifierKey(value: String): Option[InputKey] =
    value match
      case "ctrl"                     => Some(InputKey.Ctrl)
      case "alt"                      => Some(InputKey.Alt)
      case "shift"                    => Some(InputKey.Shift)
      case "meta" | "cmd" | "command" => Some(InputKey.Meta)
      case _                          => None

final case class HotkeyConfig(
    bindings: Map[HotkeyAction, List[HotkeyTrigger]] = HotkeyConfig.defaultBindings,
    // Keyed by registry command id (`Command.name`), for commands with no `HotkeyAction` of their own (issue #1922).
    // An empty list is kept rather than dropped: it records that the user unbound a shipped default, so a reload does
    // not bring the default back.
    commandBindings: Map[String, List[HotkeyTrigger]] = HotkeyConfig.defaultCommandBindings
):
  def bindingsFor(action: HotkeyAction): List[HotkeyTrigger] =
    bindings.getOrElse(action, Nil)

  def commandBindingsFor(commandId: String): List[HotkeyTrigger] =
    commandBindings.getOrElse(commandId, Nil)

  def withBinding(action: HotkeyAction, trigger: HotkeyTrigger): HotkeyConfig =
    HotkeyConfig.validated(copy(bindings = bindings + (action -> List(trigger)))).getOrElse(this)

  def withBinding(action: HotkeyAction, binding: String): HotkeyConfig =
    HotkeyTrigger.parse(binding).map(trigger => withBinding(action, trigger)).getOrElse(this)

  /** Refused, like [[withBinding]], when another action or command already holds the trigger. */
  def withCommandBinding(commandId: String, binding: String): HotkeyConfig =
    HotkeyTrigger
      .parse(binding)
      .flatMap(trigger =>
        HotkeyConfig.validated(copy(commandBindings = commandBindings + (commandId -> List(trigger)))).toOption
      )
      .getOrElse(this)

  /** Assign a trigger after removing it from every other global action and command. */
  def withBindingUnbindingConflicts(action: HotkeyAction, binding: String): HotkeyConfig =
    HotkeyTrigger.parse(binding) match
      case Some(trigger) =>
        val freed = withoutTrigger(trigger)
        HotkeyConfig.validated(freed.copy(bindings = freed.bindings + (action -> List(trigger)))).getOrElse(this)
      case None => this

  def resetBinding(action: HotkeyAction): HotkeyConfig =
    HotkeyConfig
      .validated(copy(bindings = bindings + (action -> HotkeyConfig.defaultBindings.getOrElse(action, Nil))))
      .getOrElse(this)

  /** Strips from the command bindings not in `explicitCommandIds` -- the shipped defaults a file or session did not
    * mention -- any trigger the user's own bindings already hold. A config written before a default existed may have
    * given its key to something else, and a conflict would otherwise throw away every hotkey it sets.
    */
  def yieldingDefaultCommandBindings(explicitCommandIds: Set[String]): HotkeyConfig =
    val (explicit, defaulted) = commandBindings.partition(entry => explicitCommandIds.contains(entry._1))
    val taken                 = (bindings.valuesIterator ++ explicit.valuesIterator).flatten.toSet
    copy(commandBindings = explicit ++ defaulted.view.mapValues(_.filterNot(taken.contains)).toMap)

  private def withoutTrigger(trigger: HotkeyTrigger): HotkeyConfig =
    copy(
      bindings = bindings.view.mapValues(_.filterNot(_ == trigger)).toMap,
      commandBindings = commandBindings.view.mapValues(_.filterNot(_ == trigger)).toMap
    )

  /** Rewrites every binding still at the macOS/Cmd-conditioned platform default to the Ctrl-based binding every
    * terminal actually forwards (issue #1213): a real terminal cannot deliver Cmd/Meta as an ordinary keystroke the way
    * AWT does for a focused Swing window -- macOS's own Terminal.app/iTerm2 intercept Cmd+Q as their own "quit the
    * terminal" shortcut before it ever reaches a running program's stdin, and `TerminalInputDecoder` never produces
    * `Modifier.Meta` from a plain keystroke either (only from a kitty-protocol-negotiated terminal actually choosing to
    * report it). An action the user customized away from its platform default is left untouched here -- it was
    * reachable enough for them to have bound it deliberately.
    *
    * Compares against `defaultBindingsFor`'s macOS output specifically -- never `defaultBindings`, which reads the
    * *running* JVM's `os.name` -- so this rewrite behaves identically whether Serenity's TUI is actually running on
    * macOS (the case it exists for) or is merely constructing/testing a mac-flavored `HotkeyConfig` from Linux CI.
    */
  def forTerminalUse: HotkeyConfig =
    HotkeyConfig(
      HotkeyConfig.terminalSafe(
        bindings,
        HotkeyConfig.validatedBindings(HotkeyConfig.defaultBindingsFor("Mac OS X")),
        HotkeyConfig.terminalDefaultBindings
      ),
      HotkeyConfig.terminalSafe(
        commandBindings,
        HotkeyConfig.defaultCommandBindingsFor("Mac OS X"),
        HotkeyConfig.defaultCommandBindingsFor("linux")
      )
    )

object HotkeyConfig:

  def forOs(osName: String): HotkeyConfig =
    HotkeyConfig(platformDefaults(osName), defaultCommandBindingsFor(osName))

  /** What an action holds on `osName` when the user has changed nothing: the baseline `config.conf` is written against.
    */
  def platformDefaults(osName: String): Map[HotkeyAction, List[HotkeyTrigger]] =
    validatedBindings(defaultBindingsFor(osName))

  def defaultBindings: Map[HotkeyAction, List[HotkeyTrigger]] = platformDefaults(HotkeyOverrides.runningOs)

  /** The Ctrl-based bindings [[defaultBindingsFor]] resolves to on any non-macOS `osName` -- what
    * [[HotkeyConfig.forTerminalUse]] rewrites a still-at-default macOS/Cmd binding to (issue #1213). Any non-mac string
    * works here; a literal one names the intent rather than relying on `defaultBindingsFor`'s `isMac` check failing on
    * an empty string.
    */
  def terminalDefaultBindings: Map[HotkeyAction, List[HotkeyTrigger]] =
    validatedBindings(defaultBindingsFor("linux"))

  def defaultCommandBindings: Map[String, List[HotkeyTrigger]] =
    defaultCommandBindingsFor(System.getProperty("os.name", ""))

  /** Keys for registry commands that have no [[HotkeyAction]]: the prose formatting toggles (issue #1858). A terminal
    * sends Ctrl+I as Tab, and `TerminalInputDecoder` keeps it Tab, so italic's key reaches only the GUI and terminals
    * that report modified keys distinctly -- the Tab key itself is never taken.
    */
  def defaultCommandBindingsFor(osName: String): Map[String, List[HotkeyTrigger]] =
    val primaryModifier = primaryModifierFor(osName)
    Map("bold" -> 'b', "italic" -> 'i', "underline" -> 'u').view
      .mapValues(key => List(HotkeyTrigger(InputKey.Character, Some(key), Set(primaryModifier))))
      .toMap

  private def isMac(osName: String): Boolean = osName.toLowerCase(java.util.Locale.ROOT).contains("mac")

  private def primaryModifierFor(osName: String): Modifier = if isMac(osName) then Modifier.Meta else Modifier.Ctrl

  // Plain Alt rather than the primary modifier, so it is the same on every platform with no terminal rewrite. Nothing
  // else binds Alt+Arrow, and the editor's word moves stay on Ctrl+Arrow.
  private def directionalFocusBindings: Map[HotkeyAction, List[HotkeyTrigger]] =
    Map(
      HotkeyAction.FocusLeft  -> InputKey.ArrowLeft,
      HotkeyAction.FocusRight -> InputKey.ArrowRight,
      HotkeyAction.FocusUp    -> InputKey.ArrowUp,
      HotkeyAction.FocusDown  -> InputKey.ArrowDown
    ).view.mapValues(key => List(HotkeyTrigger(key, None, Set(Modifier.Alt)))).toMap

  // Ctrl+G is Go to Line, so the chapter-note keys live on the shifted letters. None of them is Ctrl+Shift+H/I/J/M,
  // which collapse into Backspace/Tab/Enter on terminals that cannot tell Ctrl+Shift+letter from Ctrl+letter.
  private def chapterNoteBindings(primaryModifier: Modifier): Map[HotkeyAction, List[HotkeyTrigger]] =
    def shifted(key: Char): List[HotkeyTrigger] =
      List(HotkeyTrigger(InputKey.Character, Some(key), Set(primaryModifier, Modifier.Shift)))
    Map(
      HotkeyAction.ToggleChapterGhosts -> shifted('g'),
      HotkeyAction.OpenChapterNote     -> shifted('n'),
      HotkeyAction.ToggleNotesPin      -> shifted('l')
    )

  // VS Code's alternative Quick Open key: its main one, the primary modifier with P, is the command runner here.
  private def goToFileBindings(primaryModifier: Modifier): Map[HotkeyAction, List[HotkeyTrigger]] =
    Map(HotkeyAction.GoToFile -> List(HotkeyTrigger(InputKey.Character, Some('e'), Set(primaryModifier))))

  // Cmd+Shift+Z is the macOS convention, so it leads there and Cmd+Y stays as a secondary; Ctrl+Y leads elsewhere.
  private def redoBindings(osName: String, primaryModifier: Modifier): List[HotkeyTrigger] =
    val shiftedZ = HotkeyTrigger(InputKey.Character, Some('z'), Set(primaryModifier, Modifier.Shift))
    val plainY   = HotkeyTrigger(InputKey.Character, Some('y'), Set(primaryModifier))
    if isMac(osName) then List(shiftedZ, plainY) else List(plainY, shiftedZ)

  def defaultBindingsFor(osName: String): Map[HotkeyAction, List[HotkeyTrigger]] =
    val primaryModifier = primaryModifierFor(osName)
    def primary(key: Char, shift: Boolean = false, alt: Boolean = false): HotkeyTrigger =
      HotkeyTrigger(
        InputKey.Character,
        Some(key),
        Set(primaryModifier) ++ Option.when(shift)(Modifier.Shift).toSet ++ Option.when(alt)(Modifier.Alt).toSet
      )
    def primaryKey(key: InputKey, shift: Boolean = false): HotkeyTrigger =
      HotkeyTrigger(key, None, Set(primaryModifier) ++ Option.when(shift)(Modifier.Shift).toSet)
    def primaryDoubleTap: HotkeyTrigger =
      HotkeyTrigger(
        primaryModifier match
          case Modifier.Ctrl => InputKey.Ctrl
          case Modifier.Meta => InputKey.Meta
          case _             => InputKey.Unknown,
        None,
        Set.empty
      )

    Map(
      HotkeyAction.Save -> List(primary('s')),
      HotkeyAction.Quit -> List(
        primary('q'),
        HotkeyTrigger(InputKey.EOF, None, Set.empty)
      ),
      HotkeyAction.Undo      -> List(primary('z')),
      HotkeyAction.Redo      -> redoBindings(osName, primaryModifier),
      HotkeyAction.Copy      -> List(primary('c')),
      HotkeyAction.Paste     -> List(primary('v')),
      HotkeyAction.Cut       -> List(primary('x')),
      HotkeyAction.SelectAll -> List(primary('a')),
      HotkeyAction.ToggleSyntaxHighlighting -> List(
        primary('h', shift = true)
      ),
      HotkeyAction.OpenFile -> List(primary('o')),
      HotkeyAction.ToggleCommandRunner -> List(
        primary('p'),
        primaryDoubleTap
      ),
      HotkeyAction.ToggleContextualToolbar -> List(
        primary('t', shift = true)
      ),
      HotkeyAction.NewTab   -> List(primary('t')),
      HotkeyAction.CloseTab -> List(primary('w')),
      // Mirrors macOS Terminal.app/iTerm2's own Cmd+D / Cmd+Shift+D split convention -- the vertical dividing line
      // ("side by side" panes, this codebase's `SplitAxis.Horizontal`) on the plain key, the horizontal dividing line
      // ("stacked" panes, `SplitAxis.Vertical`) on its shifted variant, same shift-for-the-broader/secondary-variant
      // pattern as `FileSearch` over `Find` and `ToggleContextualToolbar` over `NewTab` below.
      HotkeyAction.SplitPaneHorizontal -> List(primary('d')),
      HotkeyAction.SplitPaneVertical   -> List(primary('d', shift = true)),
      // Shift-for-the-broader-scope variant of CloseTab, same pattern as FileSearch over Find and
      // ToggleContextualToolbar over NewTab above: closing a pane is a bigger action than closing one of its tabs.
      HotkeyAction.ClosePane -> List(primary('w', shift = true)),
      HotkeyAction.FileSearch -> List(
        primary('f', shift = true)
      ),
      HotkeyAction.NextTab -> List(primaryKey(InputKey.Tab)),
      HotkeyAction.PreviousTab -> List(
        primaryKey(InputKey.Tab, shift = true),
        primaryKey(InputKey.ReverseTab)
      ),
      // Mirrors the browser convention for moving a tab (Firefox's Ctrl+Shift+PageUp/PageDown) rather than reusing
      // NextTab/PreviousTab's Tab-key bindings, since Shift+Tab already means PreviousTab -- there is no unshifted
      // "switch tab" action on Page keys to shift-broaden the way ClosePane/SplitPaneVertical/FileSearch do over their
      // plain counterparts. Also avoids colliding with the editor's own Ctrl+Shift+Left/Right
      // (`ExtendSelectionWordLeft/Right` in `FocusedKeymapConfig`), which arrow-key "move tab" bindings would hit.
      HotkeyAction.MoveTabLeft  -> List(primaryKey(InputKey.PageUp, shift = true)),
      HotkeyAction.MoveTabRight -> List(primaryKey(InputKey.PageDown, shift = true)),
      HotkeyAction.Find         -> List(primary('f')),
      HotkeyAction.Replace      -> List(if isMac(osName) then primary('f', alt = true) else primary('h')),
      HotkeyAction.GoToLine     -> List(primary('g')),
      HotkeyAction.SaveAs       -> List(primary('s', shift = true)),
      // Plain F1, not primary-modifier-gated: unlike the Cmd/Ctrl bindings above, F1 is delivered identically by
      // every terminal and by AWT regardless of platform, so it needs none of `forTerminalUse`'s Mac-Cmd rewriting
      // (issue #1213) and no per-OS branching here.
      HotkeyAction.ToggleShortcutsHelp -> List(HotkeyTrigger(InputKey.F1, None, Set.empty))
    ) ++ directionalFocusBindings ++ goToFileBindings(primaryModifier) ++ chapterNoteBindings(primaryModifier)

  def validate(bindings: Map[HotkeyAction, List[HotkeyTrigger]]): Either[String, Unit] =
    conflictIn(actionTargets(bindings))

  /** Actions and commands share one key space: a trigger held by an action and a command, or by two commands, is as
    * much a conflict as one held by two actions.
    */
  def validate(config: HotkeyConfig): Either[String, Unit] =
    conflictIn(
      actionTargets(config.bindings) ++
        config.commandBindings.toList.sortBy(_._1).map((commandId, triggers) => s"command.$commandId" -> triggers)
    )

  private[config] def actionTargets(
    bindings: Map[HotkeyAction, List[HotkeyTrigger]]
  ): List[(String, List[HotkeyTrigger])] =
    bindings.toList.map((action, triggers) => action.configKey -> triggers)

  private def conflictIn(targets: List[(String, List[HotkeyTrigger])]): Either[String, Unit] =
    targets
      .flatMap((target, triggers) => triggers.map(_ -> target))
      .groupMap(_._1)(_._2)
      .collectFirst { case (trigger, owners) if owners.distinct.size > 1 => trigger -> owners.distinct }
      .toLeft(())
      .left
      .map {
        case (trigger, owners) =>
          "Conflicting hotkey binding '" + trigger.render + "' for " + owners.mkString(", ")
      }

  private[config] def validated(config: HotkeyConfig): Either[String, HotkeyConfig] =
    validate(config).map(_ => config)

  private def validatedBindings(
    bindings: Map[HotkeyAction, List[HotkeyTrigger]]
  ): Map[HotkeyAction, List[HotkeyTrigger]] =
    validate(bindings).fold(_ => Map.empty, _ => bindings)

  private def terminalSafe[K](
    current: Map[K, List[HotkeyTrigger]],
    macDefaults: Map[K, List[HotkeyTrigger]],
    standard: Map[K, List[HotkeyTrigger]]
  ): Map[K, List[HotkeyTrigger]] =
    current.map {
      case (key, triggers) if macDefaults.get(key).contains(triggers) => key -> standard.getOrElse(key, triggers)
      case unchanged                                                  => unchanged
    }

  private def addNonConflictingDefaults(
    bindings: Map[HotkeyAction, List[HotkeyTrigger]],
    taken: Set[HotkeyTrigger]
  ): Map[HotkeyAction, List[HotkeyTrigger]] =
    defaultBindings.foldLeft(bindings) {
      case (updated, (action, triggers)) =>
        val conflicts =
          triggers.exists(trigger => taken.contains(trigger) || updated.valuesIterator.flatten.contains(trigger))
        if updated.contains(action) || conflicts then updated
        else updated + (action -> triggers)
    }

  private val logger = LoggerFactory.getLogger("com.serenity.config.HotkeyConfig")

  // Session files keep command bindings in the same object as the action ones; no action's key contains a dot.
  private val sessionCommandPrefix = "command."

  given Encoder[HotkeyAction] = Encoder.encodeString.contramap(_.configKey)

  given Decoder[HotkeyAction] = Decoder.decodeString.emap { key =>
    HotkeyAction.values.find(_.configKey == key).toRight(s"Unknown hotkey action: $key")
  }

  given Encoder[InputKey] = Encoder.encodeString.contramap(_.toString)
  given Decoder[InputKey] =
    Decoder.decodeString.emap(key => InputKey.values.find(_.toString == key).toRight(s"Unknown input key: $key"))

  given Encoder[Modifier] = Encoder.encodeString.contramap(_.toString)
  given Decoder[Modifier] =
    Decoder.decodeString.emap(key => Modifier.values.find(_.toString == key).toRight(s"Unknown modifier: $key"))

  given Encoder[HotkeyTrigger] = deriveEncoder
  given Decoder[HotkeyTrigger] = deriveDecoder

  given Encoder[HotkeyConfig] = Encoder.instance { config =>
    val actions = config.bindings.map { case (action, triggers) => action.configKey -> triggers }
    val commands =
      config.commandBindings.map { case (commandId, triggers) => s"$sessionCommandPrefix$commandId" -> triggers }
    (actions ++ commands).asJson
  }

  given Decoder[HotkeyConfig] = Decoder.decodeMap[String, List[HotkeyTrigger]].emap { entries =>
    val (commandEntries, actionEntries) = entries.partition { case (key, _) => key.startsWith(sessionCommandPrefix) }
    val commands = commandEntries.map { case (key, triggers) => key.stripPrefix(sessionCommandPrefix) -> triggers }
    val (unknown, actions) = actionEntries.toList.partitionMap { (key, triggers) =>
      HotkeyAction.values.find(_.configKey == key).map(_ -> triggers).toRight(key)
    }
    unknown.foreach(key => logger.warn(s"[SESSION] Ignoring hotkey '$key': there is no hotkey action of that name"))
    validated(
      HotkeyConfig(
        addNonConflictingDefaults(actions.toMap, commands.valuesIterator.flatten.toSet),
        defaultCommandBindings ++ commands
      ).yieldingDefaultCommandBindings(commands.keySet)
    )
  }
