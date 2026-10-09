package com.serenity.config

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.config.AppConfigOps.*
import com.typesafe.config.ConfigFactory
import io.circe.parser.decode
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import HotkeyConfig.given

/** `config.conf` holds the user's changes to the hotkeys and nothing else. Writing every binding pinned each action to
  * the defaults of whichever build last saved the file, so a default added later (Ctrl+Shift+Z for redo, #2080) never
  * reached anyone who had saved once.
  */
class HotkeyOverridesSpec extends AnyFlatSpec with Matchers with OptionValues:

  private val linux = "linux"
  private val mac   = "Mac OS X"

  private val linuxPlan =
    ConfigMigrations.Plan(List(ConfigMigrations.droppingDefaultHotkeys(linux)), ConfigVersion.Current)

  private def withHotkeys(hotkeys: HotkeyConfig): AppConfig = AppConfig.default.withHotkeyConfig(hotkeys)

  private def hotkeyLines(config: AppConfig, osName: String): List[String] =
    ConfigFileFormat.render(config, osName).linesIterator.filter(_.startsWith("hotkey.")).toList

  private def tempFile(content: String): Path =
    val file = TestTemp.file("serenity-hotkeys", ".conf")
    Files.writeString(file, content)
    file

  private def load(text: String, plan: ConfigMigrations.Plan = linuxPlan): ConfigLoadResult =
    ConfigManager.parseConfigResult(tempFile(text), plan)

  private def resource(name: String): String =
    scala.io.Source.fromResource(name)(using scala.io.Codec.UTF8).mkString

  private def rendered(triggers: List[HotkeyTrigger]): List[String] = triggers.map(_.render)

  private val currentVersion = s"config.version = ${ConfigVersion.Current.value}\n"

  // Writing

  "a config at the platform defaults" should "write no hotkey lines at all" in {
    hotkeyLines(withHotkeys(HotkeyConfig.forOs(linux)), linux) shouldBe Nil
    hotkeyLines(withHotkeys(HotkeyConfig.forOs(mac)), mac) shouldBe Nil
  }

  "a changed action" should "be the only hotkey line written" in {
    val changed = HotkeyConfig.forOs(linux).withBinding(HotkeyAction.Save, "ctrl+alt+s")

    hotkeyLines(withHotkeys(changed), linux) shouldBe List("""hotkey.save = ["ctrl+alt+s"]""")
  }

  it should "be written with its full list when only part of the default is kept" in {
    val base = HotkeyConfig.forOs(linux)
    val changed =
      base.copy(bindings = base.bindings + (HotkeyAction.Redo -> List(base.bindingsFor(HotkeyAction.Redo).head)))

    hotkeyLines(withHotkeys(changed), linux) shouldBe List("""hotkey.redo = ["ctrl+y"]""")
  }

  "an action the user unbound" should "be written as an explicit empty list" in {
    val base    = HotkeyConfig.forOs(linux)
    val unbound = base.copy(bindings = base.bindings + (HotkeyAction.Find -> Nil))

    hotkeyLines(withHotkeys(unbound), linux) shouldBe List("hotkey.find = []")
  }

  "a command binding" should "be written only when it differs from the command's default" in {
    val base = HotkeyConfig.forOs(linux)
    val changed = base.copy(commandBindings =
      base.commandBindings + ("toggle-line-numbers" -> HotkeyTrigger.parse("ctrl+alt+l").toList)
    )

    hotkeyLines(withHotkeys(base), linux) shouldBe Nil
    hotkeyLines(withHotkeys(changed), linux) shouldBe List("""hotkey.command.toggle-line-numbers = ["ctrl+alt+l"]""")
  }

  it should "keep an explicit empty list for a default the user unbound" in {
    val base    = HotkeyConfig.forOs(linux)
    val unbound = base.copy(commandBindings = base.commandBindings + ("bold" -> Nil))

    hotkeyLines(withHotkeys(unbound), linux) shouldBe List("hotkey.command.bold = []")
  }

  it should "write a changed default of a command that is missing from the map as unbound" in {
    val base    = HotkeyConfig.forOs(linux)
    val missing = base.copy(commandBindings = base.commandBindings - "italic")

    hotkeyLines(withHotkeys(missing), linux) shouldBe List("hotkey.command.italic = []")
  }

  "the platform the defaults are taken from" should "decide what counts as a change" in {
    val macConfig = withHotkeys(HotkeyConfig.forOs(mac))

    hotkeyLines(macConfig, mac) shouldBe Nil
    hotkeyLines(macConfig, linux) should contain("""hotkey.save = ["meta+s"]""")
    hotkeyLines(withHotkeys(HotkeyConfig.forOs(linux)), mac) should contain("""hotkey.save = ["ctrl+s"]""")
  }

  it should "give macOS its own redo order, so Cmd+Shift+Z leading is not a change there" in {
    val macConfig = withHotkeys(HotkeyConfig.forOs(mac))

    macConfig.inputConfig.hotkeyConfig.bindingsFor(HotkeyAction.Redo).map(_.render) shouldBe
      List("meta+shift+z", "meta+y")
    hotkeyLines(macConfig, mac).filter(_.startsWith("hotkey.redo")) shouldBe Nil
  }

  // Loading

  "a slim file" should "load as the platform defaults plus the overrides it names" in {
    val result = load(s"""${currentVersion}hotkey.redo = ["ctrl+alt+r"]
                         |hotkey.command.bold = ["ctrl+alt+b"]
                         |""".stripMargin)
    val hotkeys  = result.config.inputConfig.hotkeyConfig
    val defaults = HotkeyConfig.forOs(linux)

    rendered(hotkeys.bindingsFor(HotkeyAction.Redo)) shouldBe List("ctrl+alt+r")
    rendered(hotkeys.commandBindingsFor("bold")) shouldBe List("ctrl+alt+b")
    hotkeys.bindings - HotkeyAction.Redo shouldBe defaults.bindings - HotkeyAction.Redo
    hotkeys.commandBindings - "bold" shouldBe defaults.commandBindings - "bold"
    result.report.hotkeyConflicts shouldBe Nil
  }

  it should "load a file with no hotkey lines as the platform defaults" in {
    load(currentVersion).config.inputConfig.hotkeyConfig shouldBe HotkeyConfig.forOs(linux)
  }

  it should "load an explicit empty list as an unbound action" in {
    val hotkeys = load(s"${currentVersion}hotkey.find = []\n").config.inputConfig.hotkeyConfig

    hotkeys.bindingsFor(HotkeyAction.Find) shouldBe Nil
    hotkeys.bindingsFor(HotkeyAction.Replace) should not be empty
  }

  it should "come back as written, for every kind of change at once" in {
    val base = HotkeyConfig.forOs(linux)
    val changed = base.copy(
      bindings = base.bindings + (HotkeyAction.Save    -> HotkeyTrigger.parse("ctrl+alt+s").toList) +
        (HotkeyAction.Find                             -> Nil),
      commandBindings = base.commandBindings + ("bold" -> Nil)
    )

    load(ConfigFileFormat.render(withHotkeys(changed), linux)).config.inputConfig.hotkeyConfig shouldBe changed
  }

  // Migration

  "the config version" should "have moved past the format that wrote every hotkey" in {
    ConfigVersion.Current shouldBe ConfigVersion(2)
  }

  "a file written before Ctrl+Shift+Z was added to redo" should "load with the current redo bindings" in {
    val result  = load(resource("compat/pre-2080-hotkeys.conf"))
    val hotkeys = result.config.inputConfig.hotkeyConfig

    rendered(hotkeys.bindingsFor(HotkeyAction.Redo)) shouldBe List("ctrl+y", "ctrl+shift+z")
    hotkeys shouldBe HotkeyConfig.forOs(linux)
    result.report.migratedFrom shouldBe Some(ConfigVersion(1))
    result.config.surfaceConfig.wordWrapEnabled shouldBe false
  }

  "a customised action in an old file" should "survive the migration" in {
    val result = load("""config.version = 1
                        |hotkey.redo = ["ctrl+alt+r"]
                        |hotkey.save = ["ctrl+s", "ctrl+alt+s"]
                        |""".stripMargin)
    val hotkeys = result.config.inputConfig.hotkeyConfig

    rendered(hotkeys.bindingsFor(HotkeyAction.Redo)) shouldBe List("ctrl+alt+r")
    rendered(hotkeys.bindingsFor(HotkeyAction.Save)) shouldBe List("ctrl+s", "ctrl+alt+s")
  }

  "a trigger the user removed from a default list before the migration" should "come back once" in {
    val hotkeys = load("config.version = 1\nhotkey.quit = [\"ctrl+q\"]\n").config.inputConfig.hotkeyConfig

    rendered(hotkeys.bindingsFor(HotkeyAction.Quit)) shouldBe List("ctrl+q", "eof")
  }

  "an empty action list in an old file" should "be read as it always was, as no override" in {
    val hotkeys = load("config.version = 1\nhotkey.find = []\n").config.inputConfig.hotkeyConfig

    rendered(hotkeys.bindingsFor(HotkeyAction.Find)) shouldBe List("ctrl+f")
  }

  "a command binding in an old file" should "keep its unbinding" in {
    val hotkeys = load("config.version = 1\nhotkey.command.bold = []\n").config.inputConfig.hotkeyConfig

    hotkeys.commandBindingsFor("bold") shouldBe Nil
  }

  "an old file written one key per line" should "migrate the same way" in {
    val source = ConfigFactory.parseString(
      "\"config.version\" = 1\n\"hotkey.redo\" = [\"ctrl+y\"]\n\"hotkey.save\" = [\"ctrl+alt+s\"]"
    )

    val migrated = ConfigMigrations.migrate(source, linuxPlan).config

    migrated.hasPath("\"hotkey.redo\"") shouldBe false
    migrated.getStringList("\"hotkey.save\"").size shouldBe 1
  }

  "the hotkey migration" should "stamp the current version and be idempotent" in {
    val source =
      ConfigFactory.parseString("config.version = 1\nhotkey.redo = [\"ctrl+y\"]\nhotkey.save = [\"ctrl+alt+s\"]")

    val once  = ConfigMigrations.migrate(source, linuxPlan)
    val twice = ConfigMigrations.migrate(once.config, linuxPlan)

    once.applied shouldBe List(ConfigVersion(1))
    once.config.getInt("config.version") shouldBe ConfigVersion.Current.value
    once.config.hasPath("hotkey.redo") shouldBe false
    twice.applied shouldBe Nil
    twice.config shouldBe once.config
    ConfigMigrations.droppingDefaultHotkeys(linux).migrate(once.config) shouldBe once.config
  }

  it should "leave a file at the current version exactly as it is" in {
    val source =
      ConfigFactory.parseString(s"config.version = ${ConfigVersion.Current.value}\nhotkey.redo = [\"ctrl+y\"]")

    ConfigMigrations.migrate(source, linuxPlan).config shouldBe source
  }

  it should "not touch the file on load" in {
    val file   = tempFile(resource("compat/pre-2080-hotkeys.conf"))
    val before = Files.readString(file)

    ConfigManager.loadConfigResultIO(Some(file.toString)).unsafeRunSync().isRight shouldBe true

    Files.readString(file) shouldBe before
  }

  it should "leave the next save to write the slim format" in {
    val file   = tempFile(resource("compat/pre-2080-hotkeys.conf"))
    val loaded = ConfigManager.parseConfigResult(file, linuxPlan).config

    val saved = ConfigManager.saveConfigWith(loaded.withWheelScrollLines(7), file, linuxPlan).unsafeRunSync()

    saved shouldBe Right(())

    val written = Files.readString(file)
    written.linesIterator.filter(_.startsWith("hotkey.")).toList shouldBe Nil
    written should include(s"config.version = ${ConfigVersion.Current.value}")
    ConfigManager.parseConfigResult(file, linuxPlan).config.inputConfig.hotkeyConfig shouldBe HotkeyConfig.forOs(linux)
  }

  // Unknown actions

  "an unknown action in config.conf" should "be ignored, keeping every other binding" in {
    val result = load(s"""${currentVersion}hotkey.teleport = ["ctrl+alt+t"]
                         |hotkey.find = ["ctrl+alt+f"]
                         |""".stripMargin)
    val hotkeys = result.config.inputConfig.hotkeyConfig

    rendered(hotkeys.bindingsFor(HotkeyAction.Find)) shouldBe List("ctrl+alt+f")
    hotkeys.bindings - HotkeyAction.Find shouldBe HotkeyConfig.forOs(linux).bindings - HotkeyAction.Find
  }

  "an unknown action in a session's hotkeyConfig" should "be ignored, keeping every other binding" in {
    val json = """{"save":[{"keyType":"Character","character":"j","modifiers":["Ctrl"]}],"teleport":[]}"""

    val decoded = decode[HotkeyConfig](json).getOrElse(fail("the session's hotkeys should still decode"))

    rendered(decoded.bindingsFor(HotkeyAction.Save)) shouldBe List("ctrl+j")
    decoded.bindingsFor(HotkeyAction.Find) should not be empty
  }

  // Reset

  "resetting an action" should "delete its override from the file rather than write the default" in {
    val file = TestTemp.file("serenity-hotkeys", ".conf")
    Files.delete(file)
    val customised = AppConfig.default.withHotkeyConfig(
      AppConfig.default.inputConfig.hotkeyConfig.withBinding(HotkeyAction.Redo, "ctrl+alt+r")
    )

    ConfigManager.saveConfigIO(customised, file).unsafeRunSync() shouldBe Right(())
    Files.readString(file) should include("""hotkey.redo = ["ctrl+alt+r"]""")

    ConfigManager.saveConfigIO(customised.resetHotkeyOverride(HotkeyAction.Redo), file).unsafeRunSync() shouldBe Right(
      ()
    )

    Files.readString(file).linesIterator.filter(_.startsWith("hotkey.")).toList shouldBe Nil
  }

  // The terminal on macOS: `forTerminalUse` rewrites the Cmd defaults to Ctrl, and that rewritten config is the one
  // the state manager saves, so what counts as a change is measured from the rewritten defaults.

  private val macTerminal = HotkeyConfig.forOs(mac).forTerminalUse

  "the terminal's rewrite of the macOS defaults" should "not be written back as the user's overrides" in {
    macTerminal.bindingsFor(HotkeyAction.Save).map(_.render) shouldBe List("ctrl+s")

    hotkeyLines(withHotkeys(macTerminal), mac) shouldBe Nil
  }

  it should "still let a change made in the terminal through" in {
    val changed = macTerminal.withBinding(HotkeyAction.Find, "ctrl+alt+f")

    hotkeyLines(withHotkeys(changed), mac) shouldBe List("""hotkey.find = ["ctrl+alt+f"]""")
  }

  it should "not reach config.conf when an unrelated setting is saved from the terminal" in {
    val file = TestTemp.file("serenity-terminal-hotkeys", ".conf")
    Files.delete(file)
    val plan = ConfigMigrations.Plan(Nil, ConfigVersion.Current, mac)

    val saved =
      ConfigManager.saveConfigWith(withHotkeys(macTerminal).withWheelScrollLines(7), file, plan).unsafeRunSync()

    saved shouldBe Right(())
    Files.readString(file).linesIterator.filter(_.startsWith("hotkey.")).toList shouldBe Nil
  }

  it should "leave an override already in the file alone when the terminal saves" in {
    val file = tempFile(s"${currentVersion}hotkey.save = [\"ctrl+s\"]\nhotkey.find = [\"meta+alt+f\"]\n")
    val plan = ConfigMigrations.Plan(Nil, ConfigVersion.Current, mac)

    val saved = ConfigManager
      .saveConfigWith(
        withHotkeys(macTerminal.withBinding(HotkeyAction.Find, "meta+alt+f")).withWheelScrollLines(7),
        file,
        plan
      )
      .unsafeRunSync()

    saved shouldBe Right(())
    Files.readString(file).linesIterator.filter(_.startsWith("hotkey.")).toList shouldBe
      List("""hotkey.save = ["ctrl+s"]""", """hotkey.find = ["meta+alt+f"]""")
  }

  "resetting an action in the terminal on macOS" should "restore the terminal's default and write no override" in {
    val changed = macTerminal.withBinding(HotkeyAction.Find, "ctrl+alt+f")

    val reset = changed.resetBinding(HotkeyAction.Find, mac)

    rendered(reset.bindingsFor(HotkeyAction.Find)) shouldBe List("ctrl+f")
    reset.terminalAdjusted shouldBe true
    hotkeyLines(withHotkeys(reset), mac) shouldBe Nil
  }

  "resetting an action on macOS outside the terminal" should "restore the Cmd default" in {
    val changed = HotkeyConfig.forOs(mac).withBinding(HotkeyAction.Find, "ctrl+alt+f")

    rendered(changed.resetBinding(HotkeyAction.Find, mac).bindingsFor(HotkeyAction.Find)) shouldBe List("meta+f")
  }
