package com.serenity.app

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Instant

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.app.LaunchReset.Moved
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LaunchResetSpec extends AnyFlatSpec with Matchers:

  private val at: Instant = Instant.ofEpochMilli(1_790_000_000_123L)

  private def write(path: Path, text: String): Path =
    Files.createDirectories(path.getParent)
    Files.writeString(path, text, StandardCharsets.UTF_8)

  private def read(path: Path): String =
    Files.readString(path, StandardCharsets.UTF_8)

  "LaunchReset.backupSuffix" should "name a backup by the kind and epoch millisecond it was taken, as the session salvage does" in {
    LaunchReset.backupSuffix(at) shouldBe "reset-1790000000123"
  }

  "LaunchReset.backUpConfig" should "move the config file aside to a timestamped sibling, keeping its content" in {
    val root   = TestTemp.directory("serenity-reset-config")
    val config = write(root.resolve("config.conf"), "theme = \"light\"\n")

    val moved = LaunchReset.backUpConfig(config, at).unsafeRunSync()

    val backup = root.resolve("config.conf.reset-1790000000123")
    moved shouldBe List(Moved(config, backup))
    Files.exists(config) shouldBe false
    read(backup) shouldBe "theme = \"light\"\n"
  }

  it should "do nothing when there is no config file" in {
    val root = TestTemp.directory("serenity-reset-config")
    LaunchReset.backUpConfig(root.resolve("config.conf"), at).unsafeRunSync() shouldBe Nil
    Files.list(root).count() shouldBe 0L
  }

  it should "fail rather than overwrite an existing backup of the same name" in {
    val root     = TestTemp.directory("serenity-reset-config")
    val config   = write(root.resolve("config.conf"), "new")
    val existing = write(root.resolve("config.conf.reset-1790000000123"), "older backup")

    LaunchReset.backUpConfig(config, at).attempt.unsafeRunSync().isLeft shouldBe true
    read(existing) shouldBe "older backup"
    read(config) shouldBe "new"
  }

  "LaunchReset.backUpSession" should "move every session file into one timestamped folder, keeping their content" in {
    val root    = TestTemp.directory("serenity-reset-session")
    val index   = write(root.resolve("session-index.json"), "{\"index\":1}")
    val pending = write(root.resolve("session-write.pending.json"), "{\"pending\":1}")
    val session = write(root.resolve("sessions").resolve("session.json"), "{\"session\":1}")

    val moved = LaunchReset.backUpSession(root, at).unsafeRunSync()

    val backup = root.resolve("session-reset-1790000000123")
    moved shouldBe List(
      Moved(index, backup.resolve("session-index.json")),
      Moved(pending, backup.resolve("session-write.pending.json")),
      Moved(root.resolve("sessions"), backup.resolve("sessions"))
    )
    List(index, pending, session).map(Files.exists(_)) shouldBe List(false, false, false)
    read(backup.resolve("session-index.json")) shouldBe "{\"index\":1}"
    read(backup.resolve("session-write.pending.json")) shouldBe "{\"pending\":1}"
    read(backup.resolve("sessions").resolve("session.json")) shouldBe "{\"session\":1}"
  }

  it should "leave everything that is not session state where it is" in {
    val root   = TestTemp.directory("serenity-reset-session")
    val config = write(root.resolve("config.conf"), "kept")
    val preset = write(root.resolve("ui-presets.json"), "kept")
    val theme  = write(root.resolve("themes").resolve("mine.conf"), "kept")
    write(root.resolve("session-index.json"), "{}")

    LaunchReset.backUpSession(root, at).unsafeRunSync()

    List(config, preset, theme).map(read) shouldBe List("kept", "kept", "kept")
  }

  it should "move only what exists, and create no backup folder when there is no session" in {
    val root = TestTemp.directory("serenity-reset-session")
    LaunchReset.backUpSession(root, at).unsafeRunSync() shouldBe Nil
    Files.exists(root.resolve("session-reset-1790000000123")) shouldBe false

    val index = write(root.resolve("session-index.json"), "{}")
    LaunchReset.backUpSession(root, at).unsafeRunSync() shouldBe List(
      Moved(index, root.resolve("session-reset-1790000000123").resolve("session-index.json"))
    )
  }
