package com.serenity.command.menu

import com.serenity.config.HotkeyConfig
import com.serenity.keystroke.{InputKey, Modifier}

object MenuMnemonics:

  /** The letter at `index` of its label that opens the item. */
  final case class Mnemonic(index: Int, char: Char)

  /** Gives each label its own letter, preferring the first letter of a word and never one in `avoid`. A label left
    * without any available letter gets none.
    */
  def assign(labels: List[String], avoid: Set[Char]): List[Option[Mnemonic]] =
    val taken        = avoid.map(_.toLower)
    val withInitials = pick(labels, taken, initialsOnly = true)(List.fill(labels.size)(None))
    pick(labels, taken ++ withInitials.flatten.map(_.char.toLower), initialsOnly = false)(withInitials)

  /** The characters the keymap binds under Alt, which a mnemonic must not shadow. */
  def altLetters(hotkeys: HotkeyConfig): Set[Char] =
    (hotkeys.bindings.values.flatten ++ hotkeys.commandBindings.values.flatten)
      .filter(trigger => trigger.keyType == InputKey.Character && trigger.modifiers.contains(Modifier.Alt))
      .flatMap(_.character)
      .map(_.toLower)
      .toSet

  private def pick(labels: List[String], taken: Set[Char], initialsOnly: Boolean)(
    current: List[Option[Mnemonic]]
  ): List[Option[Mnemonic]] =
    labels
      .zip(current)
      .foldLeft((taken, List.empty[Option[Mnemonic]])):
        case ((used, done), (_, Some(kept))) => (used, done :+ Some(kept))
        case ((used, done), (label, None)) =>
          candidates(label, initialsOnly).find(c => !used.contains(c.char.toLower)) match
            case Some(found) => (used + found.char.toLower, done :+ Some(found))
            case None        => (used, done :+ None)
      ._2

  private def candidates(label: String, initialsOnly: Boolean): List[Mnemonic] =
    label.zipWithIndex.toList
      .filter((char, index) => char.isLetterOrDigit && (!initialsOnly || index == 0 || label(index - 1) == ' '))
      .map((char, index) => Mnemonic(index, char))
