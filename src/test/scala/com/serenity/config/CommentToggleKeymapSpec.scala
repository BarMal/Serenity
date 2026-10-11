package com.serenity.config

import com.serenity.command.{CommandId, CommandRegistry}
import com.serenity.keystroke.events.RunCommand
import com.serenity.keystroke.translators.TextHotkeyConverters
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommentToggleKeymapSpec extends AnyFlatSpec with Matchers:

  private val lineCommand  = "toggle-line-comment"
  private val blockCommand = "toggle-block-comment"

  private def rendered(osName: String, commandId: String): List[String] =
    HotkeyConfig.forOs(osName).commandBindingsFor(commandId).map(_.render)

  "The comment commands" should "be registered" in {
    CommandRegistry.withToggleUI.isRegistered(CommandId(lineCommand)) shouldBe true
    CommandRegistry.withToggleUI.isRegistered(CommandId(blockCommand)) shouldBe true
  }

  they should "default to Ctrl+/ (plus the byte a terminal sends for it) and Shift+Alt+A" in {
    rendered("Linux", lineCommand) shouldBe List("ctrl+/", "ctrl+_")
    rendered("Linux", blockCommand) shouldBe List("alt+shift+a")
  }

  they should "default to Cmd+/ and Shift+Option+A on macOS" in {
    rendered("Mac OS X", lineCommand) shouldBe List("meta+/", "ctrl+_")
    rendered("Mac OS X", blockCommand) shouldBe List("alt+shift+a")
  }

  they should "take no key another action, command or editor key already has" in {
    List("Linux", "Windows 11", "Mac OS X").foreach { osName =>
      HotkeyConfig.validate(HotkeyConfig.forOs(osName)) shouldBe Right(())
    }

    val taken = EditorKeyAction.defaultBindings.values.flatten.toSet
    val ours  = List(lineCommand, blockCommand).flatMap(HotkeyConfig.forOs("Linux").commandBindingsFor)
    ours.filter(taken.contains) shouldBe Nil
  }

  they should "run from their default keystrokes" in {
    val converter =
      TextHotkeyConverters.hotkeyConverter(AppConfig.default.withHotkeyConfig(HotkeyConfig.forOs("Linux")))
    val ctrlSlash      = KeyStrokeInfo(InputKey.Character, Some('/'), Set(Modifier.Ctrl))
    val ctrlUnderscore = KeyStrokeInfo(InputKey.Character, Some('_'), Set(Modifier.Ctrl))
    val shiftAltA      = KeyStrokeInfo(InputKey.Character, Some('a'), Set(Modifier.Alt, Modifier.Shift))

    converter.lift(ctrlSlash) shouldBe Some(RunCommand(lineCommand))
    converter.lift(ctrlUnderscore) shouldBe Some(RunCommand(lineCommand))
    converter.lift(shiftAltA) shouldBe Some(RunCommand(blockCommand))
  }
