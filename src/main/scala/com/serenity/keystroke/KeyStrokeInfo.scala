package com.serenity.keystroke

enum Modifier:
  case Ctrl
  case Alt
  case Shift
  case Meta

final case class KeyStrokeInfo(
    keyType: InputKey,
    character: Option[Char],
    modifiers: Set[Modifier]
):
  def hasModifier(modifier: Modifier): Boolean = modifiers.contains(modifier)
  def hasCtrl: Boolean                         = hasModifier(Modifier.Ctrl)
  def hasAlt: Boolean                          = hasModifier(Modifier.Alt)
  def hasShift: Boolean                        = hasModifier(Modifier.Shift)
  def hasMeta: Boolean                         = hasModifier(Modifier.Meta)

object KeyStrokeInfo
