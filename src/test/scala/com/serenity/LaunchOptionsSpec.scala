package com.serenity

import java.nio.file.Path

import com.serenity.app.LaunchOptions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LaunchOptionsSpec extends AnyFlatSpec with Matchers:

  /** The parse succeeded, or the test fails with the parser's own message rather than a NoSuchElementException. */
  private def parsed(args: List[String]): LaunchOptions =
    LaunchOptions.parse(args).fold(help => fail(s"expected a successful parse, got:\n$help"), identity)

  "LaunchOptions.parse" should "accept an explicit open path" in {
    parsed(List("--open", "notes.md")).openPath shouldBe Some(Path.of("notes.md"))
  }

  it should "accept a file alias" in {
    parsed(List("--file", "notes.md")).openPath shouldBe Some(Path.of("notes.md"))
  }

  it should "accept a bare launch path" in {
    parsed(List("notes.md")).openPath shouldBe Some(Path.of("notes.md"))
  }

  it should "open nothing when no path is given" in {
    parsed(Nil).openPaths shouldBe Nil
    parsed(Nil).openPath shouldBe None
  }

  it should "accept several bare paths, as a file manager launches with a multi-file selection" in {
    parsed(List("a.md", "b.md", "c.md")).openPaths shouldBe List("a.md", "b.md", "c.md").map(Path.of(_))
  }

  it should "put the --open path ahead of the bare paths" in {
    parsed(List("--open", "a.md", "b.md", "c.md")).openPaths shouldBe List("a.md", "b.md", "c.md").map(Path.of(_))
    parsed(List("b.md", "--file", "a.md")).openPaths shouldBe List("a.md", "b.md").map(Path.of(_))
  }

  it should "keep flags working alongside several paths" in {
    val options = parsed(List("a.md", "--tui", "b.md", "--eco"))
    options.openPaths shouldBe List("a.md", "b.md").map(Path.of(_))
    options.tui shouldBe true
    options.eco shouldBe true
  }

  it should "treat the first path as the one opened at startup and the rest as extra" in {
    val options = parsed(List("a.md", "b.md"))
    options.openPath shouldBe Some(Path.of("a.md"))
    options.extraOpenPaths shouldBe List(Path.of("b.md"))
  }

  it should "reject an unrecognised flag rather than silently discarding the arguments after it" in {
    // Previously this returned openPath = None: the unknown flag was not filtered, landed at the head of the
    // match, and took `notes.md` down with it. Silently opening nothing looks like a broken editor (#1280).
    val result = LaunchOptions.parse(List("--unknown", "notes.md"))
    result.isLeft shouldBe true
    result.left.toOption.map(_.errors).getOrElse(Nil) should not be empty
  }

  it should "reject --open with no path rather than starting with no file" in {
    val result = LaunchOptions.parse(List("--open"))
    result.isLeft shouldBe true
    result.left.toOption.map(_.errors).getOrElse(Nil) should not be empty
  }

  it should "treat --help as a request rather than an error" in {
    val result = LaunchOptions.parse(List("--help"))
    result.isLeft shouldBe true
    result.left.toOption.map(_.errors).getOrElse(List("unexpected")) shouldBe empty
  }

  it should "accept --version" in {
    parsed(List("--version")).showVersion shouldBe true
  }

  it should "not set showVersion when it is absent" in {
    parsed(List("notes.md")).showVersion shouldBe false
  }

  it should "recognise --smoke-test and leave it off by default" in {
    parsed(List("--smoke-test")).smokeTest shouldBe true
    parsed(List("notes.md")).smokeTest shouldBe false
  }

  it should "use the window for --smoke-test even where the terminal would be chosen" in {
    val options = parsed(List("--smoke-test"))
    LaunchOptions.resolveTuiMode(options, env = Map.empty, stdoutIsTty = true, osName = "Linux") shouldBe false
  }

  it should "start normally, without safe mode or any reset, when none is asked for" in {
    val options = parsed(List("notes.md"))
    (options.safeMode, options.resetConfig, options.resetSession) shouldBe (false, false, false)
  }

  it should "recognise --safe-mode and its --safe alias" in {
    parsed(List("--safe-mode")).safeMode shouldBe true
    parsed(List("--safe")).safeMode shouldBe true
  }

  it should "recognise --reset-config and --reset-session independently" in {
    parsed(List("--reset-config")) shouldBe LaunchOptions(resetConfig = true)
    parsed(List("--reset-session")) shouldBe LaunchOptions(resetSession = true)
    parsed(List("--reset-config", "--reset-session", "--safe-mode", "notes.md")) shouldBe LaunchOptions(
      openPaths = List(Path.of("notes.md")),
      safeMode = true,
      resetConfig = true,
      resetSession = true
    )
  }

  it should "document safe mode and both resets in --help" in {
    val help = LaunchOptions.parse(List("--help")).left.toOption.map(_.toString).getOrElse("")
    help should include("--safe-mode")
    help should include("--reset-config")
    help should include("--reset-session")
    help should include("did not finish starting")
  }

  it should "default eco to false" in {
    parsed(List("notes.md")).eco shouldBe false
  }

  it should "recognise a bare --eco flag" in {
    parsed(List("--eco")).eco shouldBe true
  }

  it should "recognise --eco alongside an open path, regardless of order" in {
    parsed(List("--eco", "notes.md")) shouldBe LaunchOptions(
      openPaths = List(Path.of("notes.md")),
      eco = true
    )
    parsed(List("notes.md", "--eco")) shouldBe LaunchOptions(
      openPaths = List(Path.of("notes.md")),
      eco = true
    )
  }

  it should "recognise --eco alongside --open" in {
    parsed(List("--eco", "--open", "notes.md")) shouldBe LaunchOptions(
      openPaths = List(Path.of("notes.md")),
      eco = true
    )
  }

  it should "default tui and gui to false" in {
    val options = parsed(List("notes.md"))
    options.tui shouldBe false
    options.gui shouldBe false
  }

  it should "recognise a bare --tui flag" in {
    parsed(List("--tui")).tui shouldBe true
  }

  it should "recognise a bare --gui flag" in {
    parsed(List("--gui")).gui shouldBe true
  }

  it should "recognise --tui alongside an open path, regardless of order" in {
    parsed(List("--tui", "notes.md")) shouldBe LaunchOptions(
      openPaths = List(Path.of("notes.md")),
      tui = true
    )
    parsed(List("notes.md", "--tui")) shouldBe LaunchOptions(
      openPaths = List(Path.of("notes.md")),
      tui = true
    )
  }

  it should "recognise --gui alongside --open" in {
    parsed(List("--gui", "--open", "notes.md")) shouldBe LaunchOptions(
      openPaths = List(Path.of("notes.md")),
      gui = true
    )
  }

  it should "recognise --tui, --gui, and --eco together" in {
    parsed(List("--tui", "--gui", "--eco", "notes.md")) shouldBe LaunchOptions(
      openPaths = List(Path.of("notes.md")),
      eco = true,
      tui = true,
      gui = true
    )
  }

  it should "default alpha to false" in {
    parsed(List("notes.md")).alpha shouldBe false
  }

  it should "recognise a bare --alpha flag" in {
    parsed(List("--alpha")).alpha shouldBe true
  }

  it should "recognise --alpha alongside an open path, regardless of order" in {
    parsed(List("--alpha", "notes.md")) shouldBe LaunchOptions(
      openPaths = List(Path.of("notes.md")),
      alpha = true
    )
    parsed(List("notes.md", "--alpha")) shouldBe LaunchOptions(
      openPaths = List(Path.of("notes.md")),
      alpha = true
    )
  }

  it should "recognise --alpha alongside --open" in {
    parsed(List("--alpha", "--open", "notes.md")) shouldBe LaunchOptions(
      openPaths = List(Path.of("notes.md")),
      alpha = true
    )
  }

  it should "recognise --tui, --gui, --eco, and --alpha together" in {
    parsed(List("--tui", "--gui", "--eco", "--alpha", "notes.md")) shouldBe LaunchOptions(
      openPaths = List(Path.of("notes.md")),
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

  it should "default to the GUI path when a display is reachable via $DISPLAY (Linux)" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(),
      env = Map("DISPLAY" -> ":0"),
      stdoutIsTty = true,
      osName = "Linux"
    ) shouldBe false
  }

  it should "default to the GUI path when a display is reachable via $WAYLAND_DISPLAY (Linux)" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(),
      env = Map("WAYLAND_DISPLAY" -> "wayland-0"),
      stdoutIsTty = true,
      osName = "Linux"
    ) shouldBe false
  }

  it should "default to the TUI path when no display is reachable and stdout is a real terminal (Linux)" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(),
      env = Map.empty,
      stdoutIsTty = true,
      osName = "Linux"
    ) shouldBe true
  }

  it should "default to the GUI path when no display is reachable but stdout is not a terminal (Linux)" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(),
      env = Map.empty,
      stdoutIsTty = false,
      osName = "Linux"
    ) shouldBe false
  }

  it should "treat blank DISPLAY/WAYLAND_DISPLAY values as unreachable (Linux)" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(),
      env = Map("DISPLAY" -> "", "WAYLAND_DISPLAY" -> ""),
      stdoutIsTty = true,
      osName = "Linux"
    ) shouldBe true
  }

  it should "default to the GUI path on Windows regardless of X11/Wayland env vars" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(),
      env = Map.empty,
      stdoutIsTty = true,
      osName = "Windows 11"
    ) shouldBe false
  }

  it should "default to the GUI path on macOS regardless of X11/Wayland env vars" in {
    LaunchOptions.resolveTuiMode(
      LaunchOptions(),
      env = Map.empty,
      stdoutIsTty = true,
      osName = "Mac OS X"
    ) shouldBe false
  }

  "LaunchOptions.detectTuiByDefault" should "be a pure function of env, stdout-tty-ness, and OS" in {
    LaunchOptions.detectTuiByDefault(Map.empty, stdoutIsTty = true, osName = "Linux") shouldBe true
    LaunchOptions.detectTuiByDefault(Map("DISPLAY" -> ":0"), stdoutIsTty = true, osName = "Linux") shouldBe false
    LaunchOptions.detectTuiByDefault(Map.empty, stdoutIsTty = false, osName = "Linux") shouldBe false
    LaunchOptions.detectTuiByDefault(Map.empty, stdoutIsTty = true, osName = "Windows 11") shouldBe false
  }
