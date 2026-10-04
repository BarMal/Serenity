package com.serenity.command

import java.nio.file.Files

import com.serenity.config.*
import com.serenity.config.HotkeyConfig.given
import com.serenity.keystroke.events.RunCommand
import com.serenity.keystroke.translators.TextHotkeyConverters
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.session.SessionConfigCodec
import com.serenity.state.models.AppState
import com.serenity.state.reducers.{AppEffect, AppEventReducer}
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Any registry command can be bound to a global key by its id (issue #1922), and the palette shows every bound
  * command's key from the same table the dispatcher reads.
  */
class CommandKeyBindingsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val linux       = AppConfig.default.withHotkeyConfig(HotkeyConfig.forOs("Linux"))
  private val lineNumbers = "toggle-line-numbers"
  private val ctrlAltL    = KeyStrokeInfo(InputKey.Character, Some('l'), Set(Modifier.Ctrl, Modifier.Alt))
  private val ctrlB       = HotkeyTrigger(InputKey.Character, Some('b'), Set(Modifier.Ctrl))
  private val ctrlS       = HotkeyTrigger(InputKey.Character, Some('s'), Set(Modifier.Ctrl))
  // Both platforms' bold default, so a fixture that takes it means the same on whichever OS runs the suite.
  private val primaryB = List(ctrlB, ctrlB.copy(modifiers = Set(Modifier.Meta)))

  private val withLineKeys =
    linux.withHotkeyConfig(linux.inputConfig.hotkeyConfig.withCommandBinding(lineNumbers, "ctrl+alt+l"))

  private def loaded(text: String): AppConfig =
    val file = Files.createTempFile("serenity-command-keys", ".conf")
    try
      Files.writeString(file, text)
      ConfigManagerTestSupport.loadConfig(Some(file.toString))
    finally Files.deleteIfExists(file): Unit

  private def registered(id: String): Command =
    CommandRegistry.withToggleUI.findCommand(id).getOrElse(fail(s"$id is not registered"))

  private def effectsOf(id: String, state: AppState): List[AppEffect] =
    AppEventReducer.reduce(RunCommand(id), state, CommandRegistry.withToggleUI).effects

  "A command bound by id in the config file" should "run that command on its keystroke" in {
    val config = loaded(s"""hotkey.command.$lineNumbers = ["ctrl+alt+l"]""")

    TextHotkeyConverters.hotkeyConverter(config).lift(ctrlAltL) shouldBe Some(RunCommand(lineNumbers))
    effectsOf(lineNumbers, AppState.initial(config)) shouldBe List(AppEffect.ExecuteCommand(registered(lineNumbers)))
  }

  it should "not run a command outside its scope, as the palette would not offer it" in {
    effectsOf("bold", AppState.initial(linux.withAppMode(AppMode.Code))) shouldBe Nil
    effectsOf("bold", AppState.initial(linux.withAppMode(AppMode.Prose))) shouldBe
      List(AppEffect.ExecuteCommand(registered("bold")))
  }

  it should "do nothing for an id no command has" in {
    effectsOf("no-such-command", AppState.initial(linux)) shouldBe Nil
  }

  "The palette" should "show the key of a command bound by id" in {
    val runner = CommandRunner.empty.activate(CommandRegistry.withToggleUI, withLineKeys)

    runner.bindingFor(registered(lineNumbers)) shouldBe Some("ctrl+alt+l")
  }

  it should "still show the keys of commands a hotkey action performs" in {
    val runner = CommandRunner.empty.activate(CommandRegistry.withToggleUI, linux)

    runner.bindingFor(registered("save")) shouldBe Some("ctrl+s")
    runner.bindingFor(registered("go-to-file")) shouldBe Some("ctrl+e")
  }

  it should "show nothing for a command whose only key was taken by an action" in {
    val taken = HotkeyConfig.forOs("Linux").withBindingUnbindingConflicts(HotkeyAction.Find, "ctrl+b")

    CommandKeyBindings.displayed(taken).get("bold") shouldBe None
  }

  "A trigger bound to an action and a command" should "be reported as a conflict" in {
    val clashing = HotkeyConfig.forOs("Linux").copy(commandBindings = Map(lineNumbers -> List(ctrlS)))

    HotkeyConfig.validate(clashing) shouldBe
      Left("Conflicting hotkey binding 'ctrl+s' for save, command.toggle-line-numbers")
  }

  it should "be refused when assigned to the command, leaving the action its key" in {
    val config = HotkeyConfig.forOs("Linux").withCommandBinding(lineNumbers, "ctrl+s")

    config.commandBindingsFor(lineNumbers) shouldBe Nil
    config.bindingsFor(HotkeyAction.Save) shouldBe List(ctrlS)
  }

  it should "be refused when assigned to the action, so the settings editor asks before taking it" in {
    val config = HotkeyConfig.forOs("Linux")

    config.withBinding(HotkeyAction.Find, "ctrl+b") shouldBe config
  }

  it should "move to the action once the user confirms unbinding the command" in {
    val reassigned = HotkeyConfig.forOs("Linux").withBindingUnbindingConflicts(HotkeyAction.Find, "ctrl+b")

    reassigned.bindingsFor(HotkeyAction.Find) shouldBe List(ctrlB)
    reassigned.commandBindingsFor("bold") shouldBe Nil
    HotkeyConfig.validate(reassigned) shouldBe Right(())
  }

  it should "stop every hotkey from dispatching, as an action-to-action conflict does" in {
    val clashing =
      linux.withHotkeyConfig(linux.inputConfig.hotkeyConfig.copy(commandBindings = Map(lineNumbers -> List(ctrlS))))

    TextHotkeyConverters
      .hotkeyConverter(clashing)
      .isDefinedAt(KeyStrokeInfo(InputKey.Character, Some('s'), Set(Modifier.Ctrl))) shouldBe false
  }

  "Two commands bound to one trigger" should "be reported as a conflict" in {
    val clashing =
      HotkeyConfig.forOs("Linux").copy(commandBindings = Map("bold" -> List(ctrlB), "italic" -> List(ctrlB)))

    HotkeyConfig.validate(clashing) shouldBe
      Left("Conflicting hotkey binding 'ctrl+b' for command.bold, command.italic")
    val refused = HotkeyConfig.forOs("Linux").withCommandBinding("italic", "ctrl+b")
    refused.commandBindingsFor("italic").map(_.render) shouldBe List("ctrl+i")
  }

  "Command key bindings in the config file" should "survive a save and reload" in {
    val file = Files.createTempFile("serenity-command-keys", ".conf")
    try
      ConfigManagerTestSupport.saveConfig(withLineKeys, file) shouldBe true
      Files.readString(file) should include(s"""hotkey.command.$lineNumbers = ["ctrl+alt+l"]""")
      ConfigManagerTestSupport.loadConfig(Some(file.toString)).inputConfig.hotkeyConfig shouldBe
        withLineKeys.inputConfig.hotkeyConfig
    finally Files.deleteIfExists(file): Unit
  }

  it should "keep a shipped default the user unbound, rather than bring it back on reload" in {
    loaded("hotkey.command.bold = []").inputConfig.hotkeyConfig.commandBindingsFor("bold") shouldBe Nil
  }

  "A config written before command bindings existed" should "load its hotkeys unchanged" in {
    val hotkeys = loaded(
      """hotkey.find = ["ctrl+b", "meta+b"]
        |hotkey.command_palette = ["ctrl+k"]
        |""".stripMargin
    ).inputConfig.hotkeyConfig

    hotkeys.bindingsFor(HotkeyAction.Find) shouldBe primaryB
    hotkeys.bindingsFor(HotkeyAction.ToggleCommandRunner).map(_.render) shouldBe List("ctrl+k")
    hotkeys.bindingsFor(HotkeyAction.Save) shouldBe HotkeyConfig.defaultBindings(HotkeyAction.Save)
    // The newly shipped default that wanted the same key gives way to the user's own binding.
    hotkeys.commandBindingsFor("bold") shouldBe Nil
    hotkeys.commandBindingsFor("underline") shouldBe HotkeyConfig.defaultCommandBindings("underline")
    HotkeyConfig.validate(hotkeys) shouldBe Right(())
  }

  "A session's hotkeys" should "carry command bindings through a save and restore" in {
    SessionConfigCodec.decode(SessionConfigCodec.encode(withLineKeys).hcursor).inputConfig.hotkeyConfig shouldBe
      withLineKeys.inputConfig.hotkeyConfig
  }

  it should "restore a session saved before command bindings existed, giving way to its own keys" in {
    val olderSession = Json.obj("find" -> primaryB.asJson)

    val restored = olderSession.as[HotkeyConfig].getOrElse(fail("an older session's hotkeys did not decode"))

    restored.bindingsFor(HotkeyAction.Find) shouldBe primaryB
    restored.commandBindingsFor("bold") shouldBe Nil
    restored.commandBindingsFor("underline") shouldBe HotkeyConfig.defaultCommandBindings("underline")
  }

  "The prose formatting defaults" should "bind bold, italic and underline to the primary modifier" in {
    def rendered(osName: String): Map[String, List[String]] =
      HotkeyConfig.defaultCommandBindingsFor(osName).view.mapValues(_.map(_.render)).toMap

    rendered("Linux") shouldBe Map("bold" -> List("ctrl+b"), "italic" -> List("ctrl+i"), "underline" -> List("ctrl+u"))
    rendered("Mac OS X") shouldBe
      Map("bold" -> List("meta+b"), "italic" -> List("meta+i"), "underline" -> List("meta+u"))
    HotkeyConfig.forOs("Mac OS X").forTerminalUse.commandBindingsFor("bold") shouldBe List(ctrlB)
  }

  it should "name registered commands and clash with no other default" in {
    HotkeyConfig.defaultCommandBindingsFor("Linux").keySet.map(registered).map(_.name) shouldBe
      Set("bold", "italic", "underline")
    HotkeyConfig.validate(HotkeyConfig.forOs("Linux")) shouldBe Right(())
    HotkeyConfig.validate(HotkeyConfig.forOs("Mac OS X")) shouldBe Right(())

    val keymaps = FocusedKeymapConfig()
    val focusedTriggers =
      (keymaps.editor.bindings.values ++ keymaps.commandRunner.bindings.values ++ keymaps.modal.bindings.values ++
        keymaps.panel.bindings.values ++ keymaps.peek.bindings.values).flatten.toSet
    HotkeyConfig.defaultCommandBindingsFor("Linux").values.flatten.toSet.intersect(focusedTriggers) shouldBe empty
  }
