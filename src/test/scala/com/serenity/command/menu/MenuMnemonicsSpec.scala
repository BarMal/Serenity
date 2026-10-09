package com.serenity.command.menu

import com.serenity.config.{HotkeyAction, HotkeyConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MenuMnemonicsSpec extends AnyFlatSpec with Matchers:

  private def letters(labels: List[String], avoid: Set[Char] = Set.empty): List[Option[Char]] =
    MenuMnemonics.assign(labels, avoid).map(_.map(_.char))

  "Mnemonics" should "take each label's first letter when nothing clashes" in {
    letters(List("File", "Edit", "View", "Window", "Help")) shouldBe
      List(Some('F'), Some('E'), Some('V'), Some('W'), Some('H'))
  }

  it should "never give two labels the same letter, whatever the case" in {
    val assigned = letters(List("Save", "Save As", "Session", "Select All")).flatten.map(_.toLower)

    assigned.distinct shouldBe assigned
    assigned should have size 4
  }

  it should "prefer a word's initial to a letter further in" in {
    MenuMnemonics.assign(List("Close", "Close Others"), Set.empty).map(_.map(_.index)) shouldBe
      List(Some(0), Some(6))
  }

  it should "leave out a letter the keymap already binds under Alt" in {
    letters(List("File", "Edit"), avoid = Set('f')) shouldBe List(Some('i'), Some('E'))
  }

  it should "give no mnemonic to a label with no letter left" in {
    letters(List("A", "A"), Set.empty) shouldBe List(Some('A'), None)
  }

  "The Alt letters of a keymap" should "be exactly the characters bound under Alt" in {
    val linux = HotkeyConfig.forOs("Linux")

    MenuMnemonics.altLetters(linux.withBinding(HotkeyAction.Undo, "alt+u")) should contain('u')
    MenuMnemonics.altLetters(linux.withBinding(HotkeyAction.Undo, "ctrl+u")) should not contain 'u'
  }

  "The default keymaps" should "leave Alt+F, E, V, W and H free for the top-level menus" in {
    for os <- List("Linux", "Windows 11") do
      MenuMnemonics.altLetters(HotkeyConfig.forOs(os)) intersect Set('f', 'e', 'v', 'w', 'h') shouldBe empty
  }
