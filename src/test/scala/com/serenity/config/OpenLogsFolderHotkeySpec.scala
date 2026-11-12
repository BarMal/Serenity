package com.serenity.config

import com.serenity.command.{CommandId, CommandKeyBindings, CommandRegistry}
import com.serenity.keystroke.events.RunCommand
import com.serenity.keystroke.translators.TextHotkeyConverters
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** F2 opens the logs folder, so the fallback header can say where to look when a launch fell back to the terminal. */
class OpenLogsFolderHotkeySpec extends AnyFlatSpec with Matchers:

  private val platforms = List("Linux", "Windows 11", "Mac OS X")

  "Open Logs Folder" should "default to plain F2 on every platform" in
    platforms.foreach { osName =>
      HotkeyConfig.defaultBindingsFor(osName)(HotkeyAction.OpenLogsFolder).map(_.render) shouldBe List("f2")
    }

  it should "take no key another action or command already has" in
    platforms.foreach { osName =>
      val config = HotkeyConfig.forOs(osName)
      HotkeyConfig.validate(config) shouldBe Right(())
      val others = HotkeyConfig
        .defaultBindingsFor(osName)
        .removed(HotkeyAction.OpenLogsFolder)
        .valuesIterator
        .flatten
        .toList ++ config.commandBindings.valuesIterator.flatten
      others.filter(_.render == "f2") shouldBe Nil
    }

  it should "run the open-logs-folder command from F2" in {
    val converter = TextHotkeyConverters.hotkeyConverter(
      AppConfig.default.withHotkeyConfig(HotkeyConfig.forOs("Linux"))
    )

    converter.lift(KeyStrokeInfo(InputKey.F2, None, Set.empty)) shouldBe Some(RunCommand("open-logs-folder"))
  }

  it should "stand for a command the registry has" in {
    val command = CommandKeyBindings.commandFor(HotkeyAction.OpenLogsFolder)

    command shouldBe Some(CommandId("open-logs-folder"))
    command.exists(CommandRegistry.withToggleUI.isRegistered) shouldBe true
  }

  it should "be filed under View, beside the other help keys" in {
    HotkeyAction.OpenLogsFolder.purpose shouldBe HotkeyPurpose.View
    HotkeyAction.OpenLogsFolder.configKey shouldBe "open_logs_folder"
  }

end OpenLogsFolderHotkeySpec
