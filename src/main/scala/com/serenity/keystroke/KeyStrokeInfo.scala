package com.serenity.keystroke

enum Modifier:
  case Ctrl
  case Alt
  case Shift

case class KeyStrokeInfo(
    keyType: InputKey,
    character: Option[Char],
    modifiers: Set[Modifier]
):
  def hasModifier(modifier: Modifier): Boolean = modifiers.contains(modifier)
  def hasCtrl: Boolean                         = hasModifier(Modifier.Ctrl)
  def hasAlt: Boolean                          = hasModifier(Modifier.Alt)
  def hasShift: Boolean                        = hasModifier(Modifier.Shift)

object KeyStrokeInfo:

  def fromKeyStroke(keyStroke: com.googlecode.lanterna.input.KeyStroke): KeyStrokeInfo =
    import com.googlecode.lanterna.input.{KeyType => LK}
    val modifiers = Set(
      Option.when(keyStroke.isCtrlDown)(Modifier.Ctrl),
      Option.when(keyStroke.isAltDown)(Modifier.Alt),
      Option.when(keyStroke.isShiftDown)(Modifier.Shift)
    ).flatten
    val character = Option(keyStroke.getCharacter).filter(_.toInt != 0).map(_.toChar)
    val keyType = keyStroke.getKeyType match
      case LK.Character  => InputKey.Character
      case LK.Enter      => InputKey.Enter
      case LK.Backspace  => InputKey.Backspace
      case LK.Delete     => InputKey.Delete
      case LK.Escape     => InputKey.Escape
      case LK.Tab        => InputKey.Tab
      case LK.ReverseTab => InputKey.ReverseTab
      case LK.ArrowUp    => InputKey.ArrowUp
      case LK.ArrowDown  => InputKey.ArrowDown
      case LK.ArrowLeft  => InputKey.ArrowLeft
      case LK.ArrowRight => InputKey.ArrowRight
      case LK.Home       => InputKey.Home
      case LK.End        => InputKey.End
      case LK.PageUp     => InputKey.PageUp
      case LK.PageDown   => InputKey.PageDown
      case LK.F1         => InputKey.F1
      case LK.F2         => InputKey.F2
      case LK.F3         => InputKey.F3
      case LK.F4         => InputKey.F4
      case LK.F5         => InputKey.F5
      case LK.F6         => InputKey.F6
      case LK.F7         => InputKey.F7
      case LK.F8         => InputKey.F8
      case LK.F9         => InputKey.F9
      case LK.F10        => InputKey.F10
      case LK.F11        => InputKey.F11
      case LK.F12        => InputKey.F12
      case LK.EOF        => InputKey.EOF
      case _             => InputKey.Unknown
    KeyStrokeInfo(keyType, character, modifiers)
