package com.serenity.diagnostics

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LogLocationSpec extends AnyFlatSpec with Matchers:

  private val home  = Path.of(System.getProperty("java.io.tmpdir"), "serenity-home")
  private val state = Path.of(System.getProperty("java.io.tmpdir"), "xdg-state")

  "Platform.fromOsName" should "recognise Windows, macOS and everything else" in {
    Platform.fromOsName("Windows 11") shouldBe Platform.Windows
    Platform.fromOsName("Mac OS X") shouldBe Platform.MacOs
    Platform.fromOsName("Darwin") shouldBe Platform.MacOs
    Platform.fromOsName("Linux") shouldBe Platform.Other
    Platform.fromOsName("FreeBSD") shouldBe Platform.Other
  }

  "LogLocation.directory" should "use LOCALAPPDATA on Windows" in {
    val localAppData = Path.of(System.getProperty("java.io.tmpdir"), "local-app-data")
    LogLocation.directory(Platform.Windows, Map("LOCALAPPDATA" -> localAppData.toString), home) shouldBe
      localAppData.resolve("Serenity").resolve("logs")
  }

  it should "fall back to AppData/Local under the home directory on Windows without LOCALAPPDATA" in {
    LogLocation.directory(Platform.Windows, Map.empty, home) shouldBe
      home.resolve("AppData").resolve("Local").resolve("Serenity").resolve("logs")
  }

  it should "use Library/Logs/Serenity on macOS" in {
    LogLocation.directory(Platform.MacOs, Map.empty, home) shouldBe
      home.resolve("Library").resolve("Logs").resolve("Serenity")
  }

  it should "use XDG_STATE_HOME/serenity elsewhere" in {
    LogLocation.directory(Platform.Other, Map("XDG_STATE_HOME" -> state.toString), home) shouldBe
      state.resolve("serenity")
  }

  it should "use ~/.local/state/serenity when XDG_STATE_HOME is unset, blank or not absolute" in {
    val fallback = home.resolve(".local").resolve("state").resolve("serenity")
    LogLocation.directory(Platform.Other, Map.empty, home) shouldBe fallback
    LogLocation.directory(Platform.Other, Map("XDG_STATE_HOME" -> "  "), home) shouldBe fallback
    LogLocation.directory(Platform.Other, Map("XDG_STATE_HOME" -> "relative/state"), home) shouldBe fallback
  }

  "LogLocation.legacyDirectory" should "be the hidden dot folder logs used to go to" in {
    LogLocation.legacyDirectory(home) shouldBe home.resolve(".serenity")
  }

  "LogLocation.resolve" should "let a system property override the platform location" in {
    val chosen = Path.of(System.getProperty("java.io.tmpdir"), "chosen-logs")
    LogLocation.resolve(
      Platform.Other,
      Map.empty,
      home,
      property = key => Option.when(key == LogLocation.OverrideProperty)(chosen.toString)
    ) shouldBe chosen
    LogLocation.resolve(Platform.Other, Map.empty, home, _ => None) shouldBe
      LogLocation.directory(Platform.Other, Map.empty, home)
  }

  "LogMigration.migrate" should "move the old log and its archives into the new directory, leaving other files" in {
    val legacy = Files.createTempDirectory("serenity-legacy")
    val target = Files.createTempDirectory("serenity-new").resolve("logs")
    Files.writeString(legacy.resolve("serenity.log"), "current")
    Files.writeString(legacy.resolve("serenity.2026-10-01.0.log.gz"), "archive")
    Files.writeString(legacy.resolve("config.conf"), "settings")

    val moved = LogMigration.migrate(legacy, target).unsafeRunSync()

    moved.map(_.getFileName.toString).sorted shouldBe List(
      LogMigration.MigratedLogName,
      "serenity.2026-10-01.0.log.gz"
    ).sorted
    Files.readString(target.resolve(LogMigration.MigratedLogName)) shouldBe "current"
    Files.readString(target.resolve("serenity.2026-10-01.0.log.gz")) shouldBe "archive"
    Files.exists(legacy.resolve("serenity.log")) shouldBe false
    Files.readString(legacy.resolve("config.conf")) shouldBe "settings"
  }

  it should "never overwrite a file already in the new directory" in {
    val legacy = Files.createTempDirectory("serenity-legacy")
    val target = Files.createDirectories(Files.createTempDirectory("serenity-new").resolve("logs"))
    Files.writeString(legacy.resolve("serenity.log"), "old")
    Files.writeString(target.resolve(LogMigration.MigratedLogName), "kept")

    LogMigration.migrate(legacy, target).unsafeRunSync() shouldBe Nil

    Files.readString(target.resolve(LogMigration.MigratedLogName)) shouldBe "kept"
    Files.readString(legacy.resolve("serenity.log")) shouldBe "old"
  }

  it should "do nothing when there is no old directory or both are the same" in {
    val target = Files.createTempDirectory("serenity-new")
    LogMigration.migrate(target.resolve("missing"), target).unsafeRunSync() shouldBe Nil
    Files.writeString(target.resolve("serenity.log"), "same")
    LogMigration.migrate(target, target).unsafeRunSync() shouldBe Nil
    Files.readString(target.resolve("serenity.log")) shouldBe "same"
  }
