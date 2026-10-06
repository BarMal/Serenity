package com.serenity.config

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import com.typesafe.config.ConfigFactory
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2015: the config file's `config.version` is read, compared and written, rather than nominal. */
class ConfigVersioningSpec extends AnyFlatSpec with Matchers with OptionValues:

  private val baselineText = ConfigManager.configToString(AppConfig.default)

  private def tempFile(content: String): Path =
    val file = Files.createTempFile("serenity-versioning", ".conf")
    Files.writeString(file, content)
    file

  private def backups(file: Path): List[Path] =
    Files
      .list(file.getParent)
      .iterator
      .asScala
      .toList
      .filter(_.getFileName.toString.startsWith(s"${file.getFileName}.bak-"))

  private def save(config: AppConfig, file: Path, plan: ConfigMigrations.Plan): Either[ConfigError, Unit] =
    ConfigManager.saveConfigWith(config, file, plan).unsafeRunSync()

  private val flipped = !AppConfig.default.surfaceConfig.wordWrapEnabled

  private val renameOldWrap = ConfigMigrations.Step(
    ConfigVersion(1),
    source =>
      if source.hasPath("old.wrap") then
        source.withoutPath("old.wrap").withValue("editor.word_wrap", source.getValue("old.wrap"))
      else source,
    source => List("old.wrap is now editor.word_wrap").filter(_ => source.hasPath("old.wrap"))
  )

  private val toVersion2 = ConfigMigrations.Plan(List(renameOldWrap), ConfigVersion(2))

  private def statusOf(text: String, supported: Int): ConfigVersionStatus =
    ConfigVersionStatus.classify(ConfigFactory.parseString(text), ConfigVersion(supported))

  "a config's version" should "be classified as missing, older, current or newer" in {
    statusOf("a = 1", 3) shouldBe ConfigVersionStatus.Missing
    statusOf("config.version = soon", 3) shouldBe ConfigVersionStatus.Missing
    statusOf("config.version = 0", 3) shouldBe ConfigVersionStatus.Missing
    statusOf("config.version = 2", 3) shouldBe ConfigVersionStatus.Older(ConfigVersion(2))
    statusOf("config.version = 3", 3) shouldBe ConfigVersionStatus.Current
    statusOf("config.version = 7", 3) shouldBe ConfigVersionStatus.Newer(ConfigVersion(7))
  }

  it should "treat a missing version as the legacy version 1" in {
    ConfigMigrations.versionOf(ConfigFactory.parseString("a = 1")) shouldBe ConfigVersion(1)
  }

  "an older config" should "have its registered migrations applied and their warnings shown in the start-page notice" in {
    val file   = tempFile(s"config.version = 1\nold.wrap = $flipped\nfuture { unrelated = 1 }\n")
    val result = ConfigManager.parseConfigResult(file, toVersion2)

    result.config.surfaceConfig.wordWrapEnabled shouldBe flipped
    result.report.versionStatus shouldBe ConfigVersionStatus.Older(ConfigVersion(1))
    result.report.diagnostics should contain(
      ConfigDiagnostic.MigrationWarning(ConfigVersion(1), "old.wrap is now editor.word_wrap")
    )
    val notice = ConfigNotice.forLoad(file, result.report).value
    notice should include("old.wrap is now editor.word_wrap")
  }

  it should "be stamped with the current version, in place, by the next save" in {
    val file   = tempFile("# mine\nconfig.version = 1\neditor.typewriter_scrolling = false\n")
    val result = ConfigManager.parseConfigResult(file, ConfigMigrations.Plan(Nil, ConfigVersion(2)))

    save(result.config.withTypewriterScrolling(true), file, ConfigMigrations.Plan(Nil, ConfigVersion(2))) shouldBe
      Right(())

    val text = Files.readString(file)
    text should include("# mine")
    ConfigFactory.parseString(text).getInt("config.version") shouldBe 2
    backups(file) shouldBe Nil
  }

  it should "be stamped when a migration rewrote it, keeping the migrated setting" in {
    val file   = tempFile(s"config.version = 1\nold.wrap = $flipped\nfuture { unrelated = 1 }\n")
    val result = ConfigManager.parseConfigResult(file, toVersion2)

    save(
      result.config.withTypewriterScrolling(!result.config.surfaceConfig.typewriterScrollingEnabled),
      file,
      toVersion2
    ) shouldBe
      Right(())

    val written = ConfigFactory.parseFile(file.toFile)
    written.getInt("config.version") shouldBe 2
    ConfigManager.parseConfigResult(file, toVersion2).config.surfaceConfig.wordWrapEnabled shouldBe flipped
    backups(file).size shouldBe 1
  }

  "a newer config" should "load, be reported, keep its version and unknown keys on save, and be backed up first" in {
    val original = baselineText.replace("config.version = 1", "config.version = 7") + "future.key = 1\n"
    val file     = tempFile(original)
    val result   = ConfigManager.parseConfigResult(file)

    result.report.versionStatus shouldBe ConfigVersionStatus.Newer(ConfigVersion(7))
    val notice = ConfigNotice.forLoad(file, result.report).value
    notice should include("newer Serenity")
    notice should include("kept but ignored")

    save(
      result.config.withWordWrap(!result.config.surfaceConfig.wordWrapEnabled),
      file,
      ConfigMigrations.installed
    ) shouldBe
      Right(())

    val written = ConfigFactory.parseFile(file.toFile)
    written.getInt("config.version") shouldBe 7
    written.getInt("future.key") shouldBe 1
    backups(file).map(Files.readString) shouldBe List(original)
  }

  it should "never have its version stamped downwards when rewritten whole" in {
    ConfigVersioning.stamp(
      "# c\nconfig.version = 1\nx = 1\n",
      ConfigVersion(7)
    ) shouldBe "# c\nconfig.version = 7\nx = 1\n"
  }

  "a current config" should "produce no notice, and neither does one without a version" in {
    val current = tempFile(baselineText)
    val legacy  = tempFile(baselineText.replace("config.version = 1\n", ""))

    ConfigNotice.forLoad(current, ConfigManager.parseConfigResult(current).report) shouldBe None
    ConfigNotice.forLoad(legacy, ConfigManager.parseConfigResult(legacy).report) shouldBe None
    ConfigManager.parseConfigResult(current).report.versionStatus shouldBe ConfigVersionStatus.Current
    ConfigManager.parseConfigResult(legacy).report.versionStatus shouldBe ConfigVersionStatus.Missing
  }
