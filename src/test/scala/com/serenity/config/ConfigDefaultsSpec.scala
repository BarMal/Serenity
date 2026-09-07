package com.serenity.config

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the app's settings are when nobody has changed any of them.
  *
  * The values themselves are still constructor defaults, but where they can be *read* used to be nowhere in particular:
  * spread over the parameter lists of eight case classes, with `AppConfig.default` restating some of them and genuinely
  * overriding one, and no way to tell which was which without checking both. The registry answers that now, and
  * `docs/default-config.conf` is generated from it -- so changing a default shows up as a diff on a file rather than as
  * a literal buried in a constructor.
  */
class ConfigDefaultsSpec extends AnyFlatSpec with Matchers:

  private val referencePath = Paths.get("docs/default-config.conf")

  /** `AppConfig.default`'s hotkey bindings come from `HotkeyConfig()`'s own default, which reads the *running* JVM's
    * `os.name` (Cmd-based on macOS, Ctrl-based everywhere else -- intentional, see `HotkeyConfig.forTerminalUse`). The
    * committed reference file can only hold one of those, so this test pins the comparison to the Ctrl-based (non-mac)
    * bindings it was generated with -- otherwise the same source produces a different rendering, and a different test
    * failure, depending only on which OS happens to run it (issue #1213).
    */
  private val canonicalDefault = AppConfig.default.withHotkeyConfig(HotkeyConfig.forOs("linux"))

  "the generated reference config" should "match what the app ships with" in {
    val expected = ConfigFileFormat.render(canonicalDefault)
    val actual   = Files.readString(referencePath, StandardCharsets.UTF_8)

    withClue(
      "docs/default-config.conf is out of date. It is generated from AppConfig.default; regenerate it when a " +
        "default changes, so the change is visible in review. "
    )(actual shouldBe expected)
  }

  "the registry" should "know a default for every setting the file writes" in {
    ConfigRegistry.defaults.map(_._1) shouldBe ConfigRegistry.writtenKeys
    ConfigRegistry.writtenKeys.filter(key => ConfigRegistry.defaultFor(key).isEmpty) shouldBe Nil
  }

  it should "put a single changed setting back without disturbing its neighbours" in {
    val edited = AppConfig.default
      .withGutter(!AppConfig.default.surfaceConfig.showGutter)
      .withLineNumbers(!AppConfig.default.surfaceConfig.showLineNumbers)

    val restored = ConfigRegistry
      .resetToDefault(edited, "display.gutter")
      .getOrElse(fail("display.gutter is a registered setting"))

    restored.surfaceConfig.showGutter shouldBe AppConfig.default.surfaceConfig.showGutter
    restored.surfaceConfig.showLineNumbers shouldBe !AppConfig.default.surfaceConfig.showLineNumbers
  }
