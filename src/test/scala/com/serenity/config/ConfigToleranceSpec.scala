package com.serenity.config

import java.nio.file.{Files, Path}
import java.time.Instant

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.config.AppConfigOps.*
import com.typesafe.config.ConfigFactory
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Issue #2024: one invalid value in the config file costs that one setting -- not the file, and not the other
  * settings. Anything unrecognised in the file survives a save, and a file is never rewritten without a backup when the
  * rewrite would drop something the user wrote.
  */
class ConfigToleranceSpec extends AnyFlatSpec with Matchers with OptionValues with ScalaCheckPropertyChecks:

  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 200)

  private val baseline: AppConfig = AppConfig.default
    .withSyntaxHighlighting(true)
    .withLineNumbers(false)
    .withPaneHeaders(false)
    .withWordWrap(false)
    .withTypewriterScrolling(true)
    .withColumnMode(true)
    .withColumnTargetWidth(60)
    .withCursorBlinkTimeoutMillis(4000L)
    .withWordGoal(Some(50000))
    .withAutoSaveMode(AutoSaveMode.AfterDelay)
    .withAutoSaveDelayMillis(2500L)
    .withMinimumPaneWidth(24)
    .withWheelScrollLines(5)
    .withPreferredWindowSize(PreferredWindowSize(1280, 800))
    .withStatusLineSegments(List(StatusSegment.Position, StatusSegment.WordCount))
    .withHotkeyConfig(HotkeyConfig().withCommandBinding("toggle-line-numbers", "ctrl+alt+l"))

  private val baselineText: String = ConfigManager.configToString(baseline)

  private val currentVersionLine = s"config.version = ${ConfigVersion.Current.value}\n"

  private def tempFile(content: String): Path =
    val file = TestTemp.file("serenity-tolerance", ".conf")
    Files.writeString(file, content)
    file

  private def load(file: Path): ConfigLoadResult =
    ConfigManager.loadConfigResultIO(Some(file.toString)).unsafeRunSync() match
      case Right(result) => result
      case Left(error)   => fail(s"expected the load to succeed, received $error")

  private def save(config: AppConfig, file: Path): Either[ConfigError, Unit] =
    ConfigManager.saveConfigIO(config, file).unsafeRunSync()

  private def backups(file: Path): List[Path] =
    Files
      .list(file.getParent)
      .iterator
      .asScala
      .filter(_.getFileName.toString.startsWith(s"${file.getFileName}.bak-"))
      .toList

  private def keyOf(line: String): Option[String] =
    line.split("=", 2).toList match
      case key :: _ :: Nil => Some(key.trim.stripPrefix("\"").stripSuffix("\""))
      case _               => None

  private def corrupt(text: String, key: String, value: String): String =
    text.linesIterator.map(line => if keyOf(line).contains(key) then s"$key = $value" else line).mkString("\n")

  private val registered: List[ConfigField[?]] = ConfigRegistry.readOrder

  /** The preferred size is the one pair whose fallback is the default window's component, not "unset". */
  private val fallbacks: Map[String, String] = Map(
    "window.preferred.width"  -> PreferredWindowSize.Default.width.toString,
    "window.preferred.height" -> PreferredWindowSize.Default.height.toString
  )

  private val wrongValues: Gen[(String, Boolean)] =
    Gen.oneOf(
      "\"@@not a value@@\""  -> true,
      "\"\""                 -> true,
      "99999999999999999999" -> true,
      "-7.5x"                -> true,
      "[1, 2, 3]"            -> true,
      "{ nested = 1 }"       -> false
    )

  "a config file with one corrupted field" should "keep every other field, whichever field and however it is corrupted" in
    forAll(Gen.oneOf(registered), wrongValues) { (field, bad) =>
      val (wrong, staysAtKey) = bad
      val file                = tempFile(corrupt(baselineText, field.key, wrong))
      val result              = load(file)

      registered.filterNot(_.key == field.key).foreach { other =>
        withClue(s"${other.key} after corrupting ${field.key} with $wrong: ") {
          other.setting(result.config)._2.rendered shouldBe other.setting(baseline)._2.rendered
        }
      }
      if staysAtKey && field.codec
            .parse(FieldCodec.flatten(ConfigFactory.parseString(s"v = $wrong").getValue("v")))
            .isEmpty
      then
        result.report.invalidEntries.map(_.key) should contain(field.key)
        field.setting(result.config)._2.rendered shouldBe fallbacks.getOrElse(
          field.key,
          field.setting(AppConfig.default)._2.rendered
        )
      Files.deleteIfExists(file): Unit
    }

  it should "keep the other fields' valid hotkeys when one hotkey is not a key binding" in {
    val file = tempFile(s"$baselineText\nhotkey.save = [not-a-real-trigger]\n")

    val result = load(file)

    result.config.inputConfig.hotkeyConfig.commandBindingsFor("toggle-line-numbers").map(_.render) shouldBe
      List("ctrl+alt+l")
    result.report.invalidEntries.map(_.key) should contain("hotkey.save")
    result.config.inputConfig.hotkeyConfig.bindingsFor(HotkeyAction.Save) shouldBe
      HotkeyConfig().bindingsFor(HotkeyAction.Save)
  }

  it should "keep every other hotkey when two hotkeys claim the same trigger" in {
    val file = tempFile(
      s"""$baselineText
         |hotkey.save = "ctrl+alt+9"
         |hotkey.command.toggle-line-numbers = "ctrl+alt+9"
         |hotkey.command.bold = "ctrl+alt+8"
         |""".stripMargin
    )

    val result  = load(file)
    val hotkeys = result.config.inputConfig.hotkeyConfig

    hotkeys.bindingsFor(HotkeyAction.Save).map(_.render) shouldBe List("ctrl+alt+9")
    hotkeys.commandBindingsFor("toggle-line-numbers") shouldBe Nil
    hotkeys.commandBindingsFor("bold").map(_.render) shouldBe List("ctrl+alt+8")
    result.report.hotkeyConflicts shouldBe List(
      HotkeyConflict("ctrl+alt+9", "save", List("command.toggle-line-numbers"))
    )
    HotkeyConfig.validate(hotkeys) shouldBe Right(())
  }

  "a preferred window size with one unusable component" should "keep the other, and report the bad one" in {
    val wide = load(tempFile("window.preferred.width = very-wide\nwindow.preferred.height = 900\n"))
    val tall = load(tempFile("window.preferred.width = 1400\nwindow.preferred.height = tall\n"))
    val both = load(tempFile("window.preferred.width = very-wide\nwindow.preferred.height = tall\n"))

    wide.config.preferredWindowSize shouldBe Some(PreferredWindowSize(PreferredWindowSize.Default.width, 900))
    wide.report.invalidEntries.map(_.key) shouldBe List("window.preferred.width")
    tall.config.preferredWindowSize shouldBe Some(PreferredWindowSize(1400, PreferredWindowSize.Default.height))
    tall.report.invalidEntries.map(_.key) shouldBe List("window.preferred.height")
    both.config.preferredWindowSize shouldBe None
    both.report.invalidEntries.map(_.key) shouldBe List("window.preferred.height", "window.preferred.width")
  }

  "an invalid value" should "name the key, the bad value and why, without failing the load" in {
    val file = tempFile(
      """editor.word_wrap = false
        |typography.code.size = banana
        |cursor.info_bar.placement = floating
        |""".stripMargin
    )

    val result = load(file)

    result.config.surfaceConfig.wordWrapEnabled shouldBe false
    result.config.editorConfig.fontConfig.fontSize shouldBe AppConfig.default.editorConfig.fontConfig.fontSize
    val entry = result.report.invalidEntries.find(_.key == "typography.code.size").value
    entry.value shouldBe "banana"
    entry.reason should not be empty
    result.report.diagnostics should contain(
      ConfigDiagnostic.InvalidValue("typography.code.size", "banana", entry.reason)
    )
  }

  it should "reach the start page notice as the key, the value and the reason" in {
    val file   = tempFile("typography.code.size = banana\neditor.word_wrap = false\n")
    val result = load(file)

    val notice = ConfigNotice.forLoad(file, result.report).value

    notice should include("typography.code.size")
    notice should include("banana")
    notice should include(result.report.invalidEntries.head.reason)
    notice should include("untouched")
  }

  it should "produce no notice for a clean file" in {
    ConfigNotice.forLoad(Path.of("config.conf"), load(tempFile(baselineText)).report) shouldBe None
  }

  "a config file that cannot be parsed" should "fall back wholesale, name why, and never be overwritten by a save" in {
    val content = "editor.word_wrap = false\nthis is not = valid = hocon {{{\n"
    val file    = tempFile(content)

    val outcome          = ConfigManager.loadConfigResultIO(Some(file.toString)).unsafeRunSync()
    val (config, notice) = ConfigNotice.forOutcome(file, outcome)

    outcome.isLeft shouldBe true
    config.config shouldBe AppConfig.default
    notice.value should include("could not be parsed")
    notice.value should include("untouched")

    save(baseline, file).isLeft shouldBe true
    Files.readString(file) shouldBe content
  }

  "saving a change to a file with an invalid value" should "leave the invalid line exactly as the user wrote it" in {
    val original = corrupt(baselineText, "editor.word_wrap", "maybe")
    val file     = tempFile(original)

    save(load(file).config.withLineNumbers(true), file) shouldBe Right(())

    Files.readString(file).linesIterator.toList should contain("editor.word_wrap = maybe")
    load(file).report.invalidEntries.map(_.key) should contain("editor.word_wrap")
    load(file).config.surfaceConfig.showLineNumbers shouldBe true
    backups(file) shouldBe Nil
  }

  it should "back the file up with a timestamp before replacing the invalid value itself" in {
    val original = corrupt(baselineText, "editor.word_wrap", "maybe")
    val file     = tempFile(original)

    val changed = !AppConfig.default.surfaceConfig.wordWrapEnabled

    save(load(file).config.withWordWrap(changed), file) shouldBe Right(())

    backups(file).map(Files.readString) shouldBe List(original)
    val reloaded = load(file)
    reloaded.report.invalidEntries shouldBe Nil
    reloaded.config.surfaceConfig.wordWrapEnabled shouldBe changed
  }

  it should "not backup or touch anything extra when the file was clean" in {
    val file = tempFile(baselineText)

    save(load(file).config, file) shouldBe Right(())

    backups(file) shouldBe Nil
  }

  "backing up a config" should "never replace an earlier backup" in {
    val file = tempFile("a = 1\n")
    val t1   = Instant.parse("2026-10-05T10:00:00Z")
    val t2   = Instant.parse("2026-10-05T10:00:01Z")

    val first = ConfigManager.backUpConfig(file, t1).value
    Files.writeString(file, "a = 2\n")
    val second = ConfigManager.backUpConfig(file, t2).value

    first should not be second
    Files.readString(first) shouldBe "a = 1\n"
    Files.readString(second) shouldBe "a = 2\n"
    ConfigManager.backUpConfig(file, t2) shouldBe None
    Files.readString(second) shouldBe "a = 2\n"
  }

  "saving" should "keep settings this version does not recognise, as found" in {
    val file = tempFile(
      s"""$baselineText
         |future.feature.enabled = true
         |future.feature.names = ["a", "b c"]
         |future.feature.level = 3
         |""".stripMargin
    )

    save(load(file).config.withWordWrap(true), file) shouldBe Right(())

    val written = ConfigFactory.parseFile(file.toFile)
    written.getBoolean("future.feature.enabled") shouldBe true
    written.getStringList("future.feature.names").asScala.toList shouldBe List("a", "b c")
    written.getInt("future.feature.level") shouldBe 3
    load(file).config.surfaceConfig.wordWrapEnabled shouldBe true
  }

  "a config from a newer version" should "be reported, backed up before a save, and keep its unknown keys" in {
    val original = s"$baselineText\nconfig.version = 7\nfuture.key = 1\n"
    val file     = tempFile(original.replace(currentVersionLine, ""))
    val result   = load(file)

    result.report.version.value shouldBe 7
    result.report.newerThanSupported shouldBe true
    result.report.diagnostics.exists {
      case ConfigDiagnostic.NewerFileVersion(found, _) => found.value == 7
      case _                                           => false
    } shouldBe true
    ConfigNotice.forLoad(file, result.report).value should include("newer")

    save(result.config.withWordWrap(true), file) shouldBe Right(())

    backups(file).size shouldBe 1
    ConfigFactory.parseFile(file.toFile).getInt("future.key") shouldBe 1
  }

  "a config with an unusable version" should "be read as an unversioned file and flagged" in {
    val result = load(tempFile("config.version = soon\n"))

    result.report.version shouldBe ConfigVersionStatus.legacy
    result.report.invalidEntries.map(_.key) should contain("config.version")
  }

  "the migration hook" should "run every step from the file's version up to the target, in order, and stamp it" in {
    val steps = List(
      ConfigMigrations.Step(ConfigVersion(2), _.withFallback(ConfigFactory.parseString("second = true"))),
      ConfigMigrations.Step(
        ConfigVersion(1),
        config => ConfigFactory.parseString(s"first = ${config.getInt("old")}").withFallback(config)
      ),
      ConfigMigrations.Step(ConfigVersion(3), _ => fail("a step from the target version must not run"))
    )

    val outcome = ConfigMigrations.migrate(
      ConfigFactory.parseString("config.version = 1\nold = 5"),
      steps,
      ConfigVersion(3)
    )

    outcome.found shouldBe ConfigVersion(1)
    outcome.applied shouldBe List(ConfigVersion(1), ConfigVersion(2))
    outcome.config.getInt("first") shouldBe 5
    outcome.config.getBoolean("second") shouldBe true
    outcome.config.getInt("config.version") shouldBe 3
  }

  it should "leave a file already at the current version alone" in {
    val source = ConfigFactory.parseString("config.version = 1\nold = 5")
    val outcome = ConfigMigrations.migrate(
      source,
      List(ConfigMigrations.Step(ConfigVersion(1), _ => fail("ran"))),
      ConfigVersion(1)
    )

    outcome.applied shouldBe Nil
    outcome.config shouldBe source
  }

  "saving into a hand-written config" should "change only the line of the changed setting" in {
    val original =
      currentVersionLine + """# My Serenity setup -- keep this note
        |
        |editor.word_wrap = false   # I prefer long lines
        |  display.line_numbers = true
        |
        |# fonts
        |typography.code.size = 15.5
        |custom.plugin.level = 3
        |""".stripMargin
    val file = tempFile(original)

    save(load(file).config.withLineNumbers(false), file) shouldBe Right(())

    Files.readString(file) shouldBe original.replace("  display.line_numbers = true", "  display.line_numbers = false")
    backups(file) shouldBe Nil
  }

  it should "write nothing when nothing changed" in {
    val original = currentVersionLine + "# comment\neditor.word_wrap = false\n"
    val file     = tempFile(original)
    val before   = Files.getLastModifiedTime(file)

    save(load(file).config, file) shouldBe Right(())

    Files.readString(file) shouldBe original
    Files.getLastModifiedTime(file) shouldBe before
  }

  it should "add a setting the file did not state at the end, leaving the rest alone" in {
    val original = currentVersionLine + "# comment\neditor.word_wrap = false\n"
    val file     = tempFile(original)

    save(load(file).config.withLineNumbers(false), file) shouldBe Right(())

    Files.readString(file) shouldBe s"${original}editor.line_numbers = false\n"
  }

  it should "keep the user's spelling of a key and their line endings" in {
    val original = currentVersionLine.replace("\n", "\r\n") + "# note\r\ndisplay.word.wrap = false\r\n"
    val file     = tempFile(original)

    save(load(file).config.withWordWrap(true), file) shouldBe Right(())

    Files.readString(file) shouldBe currentVersionLine.replace("\n", "\r\n") + "# note\r\ndisplay.word.wrap = true\r\n"
  }

  it should "still take effect when the setting is also written as a block further down" in {
    val original = currentVersionLine + "editor.word_wrap = true\neditor {\n  word_wrap = true\n}\n"
    val file     = tempFile(original)

    save(load(file).config.withWordWrap(false), file) shouldBe Right(())

    load(file).config.surfaceConfig.wordWrapEnabled shouldBe false
    Files.readString(file) should startWith(currentVersionLine + "editor.word_wrap = ")
  }
