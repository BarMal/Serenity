package com.serenity.keystroke

import com.googlecode.lanterna.input.{KeyStroke, KeyType}

enum Modifier:
  case Ctrl
  case Alt
  case Shift

case class KeyStrokeInfo(
    keyType: KeyType,
    character: Option[Char],
    modifiers: Set[Modifier]
):
  def hasModifier(modifier: Modifier): Boolean = modifiers.contains(modifier)
  def hasCtrl: Boolean                         = hasModifier(Modifier.Ctrl)
  def hasAlt: Boolean                          = hasModifier(Modifier.Alt)
  def hasShift: Boolean                        = hasModifier(Modifier.Shift)

object KeyStrokeInfo:

  def fromKeyStroke(keyStroke: KeyStroke): KeyStrokeInfo =
    val modifiers = Set(
      Option.when(keyStroke.isCtrlDown)(Modifier.Ctrl),
      Option.when(keyStroke.isAltDown)(Modifier.Alt),
      Option.when(keyStroke.isShiftDown)(Modifier.Shift)
    ).flatten

    val character = Option(keyStroke.getCharacter).filter(_.toInt != 0).map(_.toChar)

    KeyStrokeInfo(
      keyType = keyStroke.getKeyType,
      character = character,
      modifiers = modifiers
    )
