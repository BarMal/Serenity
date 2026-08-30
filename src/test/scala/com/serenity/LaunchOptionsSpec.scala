package com.serenity

import java.nio.file.Path

import com.serenity.app.LaunchOptions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LaunchOptionsSpec extends AnyFlatSpec with Matchers:

  "LaunchOptions.parse" should "accept an explicit open path" in {
    LaunchOptions.parse(List("--open", "notes.md")).openPath shouldBe Some(Path.of("notes.md"))
  }

  it should "accept a file alias" in {
    LaunchOptions.parse(List("--file", "notes.md")).openPath shouldBe Some(Path.of("notes.md"))
  }

  it should "accept a bare launch path" in {
    LaunchOptions.parse(List("notes.md")).openPath shouldBe Some(Path.of("notes.md"))
  }

  it should "ignore unsupported options" in {
    LaunchOptions.parse(List("--unknown", "notes.md")).openPath shouldBe None
  }

  it should "default eco to false" in {
    LaunchOptions.parse(List("notes.md")).eco shouldBe false
  }

  it should "recognise a bare --eco flag" in {
    LaunchOptions.parse(List("--eco")).eco shouldBe true
  }

  it should "recognise --eco alongside an open path, regardless of order" in {
    LaunchOptions.parse(List("--eco", "notes.md")) shouldBe LaunchOptions(
      openPath = Some(Path.of("notes.md")),
      eco = true
    )
    LaunchOptions.parse(List("notes.md", "--eco")) shouldBe LaunchOptions(
      openPath = Some(Path.of("notes.md")),
      eco = true
    )
  }

  it should "recognise --eco alongside --open" in {
    LaunchOptions.parse(List("--eco", "--open", "notes.md")) shouldBe LaunchOptions(
      openPath = Some(Path.of("notes.md")),
      eco = true
    )
  }

  it should "default tui and gui to false" in {
    val options = LaunchOptions.parse(List("notes.md"))
    options.tui shouldBe false
    options.gui shouldBe false
  }

  it should "recognise a bare --tui flag" in {
    LaunchOptions.parse(List("--tui")).tui shouldBe true
  }

  it should "recognise a bare --gui flag" in {
    LaunchOptions.parse(List("--gui")).gui shouldBe true
  }

  it should "recognise --tui alongside an open path, regardless of order" in {
    LaunchOptions.parse(List("--tui", "notes.md")) shouldBe LaunchOptions(
      openPath = Some(Path.of("notes.md")),
      tui = true
    )
    LaunchOptions.parse(List("notes.md", "--tui")) shouldBe LaunchOptions(
      openPath = Some(Path.of("notes.md")),
      tui = true
    )
  }

  it should "recognise --gui alongside --open" in {
    LaunchOptions.parse(List("--gui", "--open", "notes.md")) shouldBe LaunchOptions(
      openPath = Some(Path.of("notes.md")),
      gui = true
    )
  }

  it should "recognise --tui, --gui, and --eco together" in {
    LaunchOptions.parse(List("--tui", "--gui", "--eco", "notes.md")) shouldBe LaunchOptions(
      openPath = Some(Path.of("notes.md")),
      eco = true,
      tui = true,
      gui = true
    )
  }

  it should "default alpha to false" in {
    LaunchOptions.parse(List("notes.md")).alpha shouldBe false
  }

  it should "recognise a bare --alpha flag" in {
    LaunchOptions.parse(List("--alpha")).alpha shouldBe true
  }

  it should "recognise --alpha alongside an open path, regardless of order" in {
    LaunchOptions.parse(List("--alpha", "notes.md")) shouldBe LaunchOptions(
      openPath = Some(Path.of("notes.md")),
      alpha = true
    )
    LaunchOptions.parse(List("notes.md", "--alpha")) shouldBe LaunchOptions(
      openPath = Some(Path.of("notes.md")),
      alpha = true
    )
  }

  it should "recognise --alpha alongside --open" in {
    LaunchOptions.parse(List("--alpha", "--open", "notes.md")) shouldBe LaunchOptions(
      openPath = Some(Path.of("notes.md")),
      alpha = true
    )
  }

  it should "recognise --tui, --gui, --eco, and --alpha together" in {
    LaunchOptions.parse(List("--tui", "--gui", "--eco", "--alpha", "notes.md")) shouldBe LaunchOptions(
      openPath = Some(Path.of("notes.md")),
      eco = true,
      tui = true,
      gui = true,
      alpha = true
    )
  }

  "LaunchOptions.resolveTuiMode" should "force the GUI path when --gui is passed, even with no display" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(gui = true),
      env = Map.empty,
      stdoutIsTty = true
    ) shouldBe false
  }

  it should "let --gui win when both --tui and --gui are passed" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(tui = true, gui = true),
      env = Map("DISPLAY" -> ":0"),
      stdoutIsTty = true
    ) shouldBe false
  }

  it should "force the TUI path when --tui is passed, even with a display reachable" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(tui = true),
      env = Map("DISPLAY" -> ":0"),
      stdoutIsTty = false
    ) shouldBe true
  }

  it should "default to the GUI path when a display is reachable via $DISPLAY" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(),
      env = Map("DISPLAY" -> ":0"),
      stdoutIsTty = true
    ) shouldBe false
  }

  it should "default to the GUI path when a display is reachable via $WAYLAND_DISPLAY" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(),
      env = Map("WAYLAND_DISPLAY" -> "wayland-0"),
      stdoutIsTty = true
    ) shouldBe false
  }

  it should "default to the TUI path when no display is reachable and stdout is a real terminal" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(),
      env = Map.empty,
      stdoutIsTty = true
    ) shouldBe true
  }

  it should "default to the GUI path when no display is reachable but stdout is not a terminal" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(),
      env = Map.empty,
      stdoutIsTty = false
    ) shouldBe false
  }

  it should "treat blank DISPLAY/WAYLAND_DISPLAY values as unreachable" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(),
      env = Map("DISPLAY" -> "", "WAYLAND_DISPLAY" -> ""),
      stdoutIsTty = true
    ) shouldBe true
  }

  "LaunchOptions.detectTuiByDefault" should "be a pure function of env and stdout-tty-ness" in {
    LaunchOptions.detectTuiByDefault(Map.empty, stdoutIsTty = true) shouldBe true
    LaunchOptions.detectTuiByDefault(Map("DISPLAY" -> ":0"), stdoutIsTty = true) shouldBe false
    LaunchOptions.detectTuiByDefault(Map.empty, stdoutIsTty = false) shouldBe false
  }
