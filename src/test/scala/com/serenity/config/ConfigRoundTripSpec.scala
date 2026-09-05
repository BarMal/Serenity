package com.serenity.config

import java.awt.Color
import java.nio.file.{Files, Path}

import scala.concurrent.duration.DurationInt

import cats.effect.unsafe.implicits.global
import com.serenity.animation.{AnimationConfig, TransitionKind, WindowSitterAction, WindowSitterConfig}
import com.serenity.keystroke.Modifier
import com.serenity.state.models.SurfacePlacement
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.typesafe.config.ConfigFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The config format's two halves -- the text `ConfigManager` writes and the parser that reads it back -- are
  * hand-maintained and were held to each other by nothing at all, so a setting could be added to [[AppConfig]], given a
  * settings-surface row, and still never survive a restart. Nine had.
  *
  * These are the tests that hold them together: everything the key schema says the format understands must actually be
  * written, everything written must parse, and a config with every field moved off its default must come back
  * unchanged.
  */
class ConfigRoundTripSpec extends AnyFlatSpec with Matchers:

  /** Alias keys differ only in `.` versus `_` between words (`display.word_wrap` / `display.word.wrap`), so a written
    * file only has to carry one spelling of each. Collapsing both separators is what makes "is this key covered?" a
    * question about the setting rather than about which spelling the writer happened to pick.
    */
  private def keyClass(key: String): String = key.replace(".", "").replace("_", "").toLowerCase

  private def savedText(config: AppConfig): String =
    val file = Files.createTempFile("serenity-config-round-trip", ".conf")
    try
      ConfigManager.saveConfig(config, file) shouldBe true
      Files.readString(file)
    finally Files.deleteIfExists(file): Unit

  private def savedAndReloaded(config: AppConfig): AppConfig =
    val file = Files.createTempFile("serenity-config-round-trip", ".conf")
    try
      ConfigManager.saveConfig(config, file) shouldBe true
      ConfigManager.loadConfig(Some(file.toString))
    finally Files.deleteIfExists(file): Unit

  /** The keys a saved file actually assigns, read off the text rather than through a parser -- these tests have to be
    * able to describe a file that does not parse.
    */
  private def writtenKeys(text: String): Set[String] =
    text.linesIterator
      .map(_.trim)
      .filterNot(line => line.isEmpty || line.startsWith("#"))
      .flatMap(line => line.split("=", 2).headOption)
      .map(_.trim.stripPrefix("\"").stripSuffix("\""))
      .filter(_.matches("[A-Za-z0-9_.]+"))
      .toSet

  /** Every field this suite knows how to move off its default. Anything reachable from the settings surface belongs
    * here: a setting missing from this list is a setting nothing checks the persistence of.
    */
  private val mutated: AppConfig = AppConfig.default
    .withSyntaxHighlighting(true)
    .withLineNumbers(false)
    .withGutter(false)
    .withPaneHeaders(false)
    .withWordCount(true)
    .withCommentDisplayMode(CommentDisplayMode.Margin)
    .withWordWrap(false)
    .withVisualLineCursorNavigation(false)
    .withTypewriterScrolling(true)
    .withFocusedTextBody(true)
    .withContextualToolbarEnabled(false)
    .withContextualToolbarDisplayMode(ToolbarDisplayMode.IconOnly)
    .withBlurRadius(0.42f)
    .withBackgroundStyle(BackgroundStyle.Solid)
    .withPostProcessingEffect(PostProcessingEffect.Scanlines)
    .withUiShadowsEnabled(false)
    .withCommandRunnerVisibleRows(Some(11))
    .withCommandRunnerItemGapRows(1.5)
    .withCommandRunnerCursorGapRows(Some(2.0))
    .withCommandRunnerShowKeyHints(false)
    .withCommandRunnerCursorPeekEnabled(true)
    .withCommandRunnerCursorPeekModifier(Modifier.Ctrl)
    .withCommandRunnerCursorPeekTapWindowMillis(300L)
    .withCommandRunnerCursorPeekPlacement(SurfacePlacement.AboveCursor)
    .withRenderFpsTarget(RenderFpsTarget.Fps30)
    .withRenderDamageGranularity(RenderDamageGranularity.Cells)
    .withCursorInfoBarBackgroundAlpha(Some(0.5))
    .withCursorMode(CursorMode.Breathe)
    .withCursorInfoBarSegments(List(CursorInfoBarSegment.Position, CursorInfoBarSegment.WordCount))
    .withCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom)
    .withMarkdownViewMode(MarkdownViewMode.SplitPreview)
    .withDefaultDocumentMode(DefaultDocumentMode.Markdown)
    .withAppMode(AppMode.Prose)
    .withShowAllSettingsRegardlessOfMode(true)
    .withInterfaceDensity(InterfaceDensity.Compact)
    .withUiElementGap(2.0)
    .withUiCornerRadiusPx(6)
    .withUiOutlineThicknessPx(3)
    .withTextAreaLeftInset(12.0)
    .withTextAreaRightInset(13.0)
    .withTextAreaTopInset(4.0)
    .withTextAreaBottomInset(5.0)
    .withMinimumPaneWidth(24)
    .withWindowChromeMode(WindowChromeMode.Native)
    .withMaterialPreset(MaterialPreset.Clear)
    .withCharacterAnimation(AnimationConfig(steps = 9, totalDuration = 210.milliseconds))
    .withFontConfig(
      FontConfig(
        codeFontFamily = "Iosevka",
        textFontFamily = "Charter",
        uiFontFamily = "Inter",
        fontSize = 15.0f,
        textFontSize = 16.0f,
        uiFontSize = 13.0f,
        enableLigatures = false,
        textLigatures = false,
        uiLigatures = true,
        textScaleMode = FontLoader.TextScaleMode.Manual,
        textScaleMultiplier = 1.25
      )
    )
    .withCursorColors(
      CursorColorConfig(active = Some(Color(0x11, 0x22, 0x33)), inactive = Some(Color(0x44, 0x55, 0x66)))
    )
    .withCursorInfoBarColors(
      CursorInfoBarColorConfig(foreground = Some(Color(0x77, 0x88, 0x99)), background = Some(Color(0xaa, 0xbb, 0xcc)))
    )
    .withSpellCheck(
      SpellCheckConfig(
        enabled = true,
        languages = List("en", "fr"),
        dictionaryPaths = List("/tmp/words.dic"),
        additionalWords = List("Serenity", "scalafix")
      )
    )
    .withElementTransitionSpeedScale(1.4)
    .withEditorTextTransitionSpeedScale(Some(1.1))
    .withCommandRunnerTransitionSpeedScale(Some(1.2))
    .withUiTransitionSpeedScale(Some(1.3))
    .withCursorTransitionSpeedScale(Some(0.9))
    .withMotionPreset(MotionPreset.Expressive)
    .withEditorInsertionTransitionKind(TransitionKind.DirectionalSweep)
    .withCommandRunnerTransitionKind(Some(TransitionKind.DirectionalSweep))
    .withPanelOpenTransitionKind(Some(TransitionKind.TypedText))
    .withPanelCloseTransitionKind(Some(TransitionKind.DirectionalSweep))
    .withCommandRunnerAnimation(Some(AnimationConfig(steps = 7, totalDuration = 140.milliseconds)))
    .withUiAnimation(Some(AnimationConfig(steps = 6, totalDuration = 130.milliseconds)))
    .withViewportWidthSizing(ViewportAxisSizing(percent = 0.8, maxCells = Some(120)))
    .withViewportHeightSizing(ViewportAxisSizing(percent = 0.9, maxCells = Some(60)))
    .withPreferredWindowSize(PreferredWindowSize(1280, 800))
    .withWheelScrollLines(5)
    .withWindowSitterConfig(
      WindowSitterConfig(
        enabled = false,
        action = WindowSitterAction.Blink,
        frames = Vector("-", "+"),
        activeTicks = 9,
        fastActiveTicks = 17,
        fastTypingThresholdMs = 175
      )
    )

  /** Fields that are mirrors of the motion hierarchy rather than settings in their own right: `motionConfiguration` and
    * its families are what the file carries, and `AppConfig`'s `effectiveMotion*` accessors resolve behaviour from
    * that. The legacy fields stay in the model for configs written before the hierarchy existed, so a saved file
    * legitimately reconstructs the hierarchy and leaves them at their defaults. What has to survive is the behaviour,
    * which the effective-motion test below asserts directly.
    */
  private val supersededByMotionHierarchy: Set[String] = Set(
    // The editor-text family's animation and this field are one setting under two names (`withEditorTextAnimation`
    // writes both). They agree for any config a user has actually touched; they differ only in the shipped default,
    // where the field is "no character animation" and the hierarchy derives the baseline preset's -- so the value
    // reloaded there is the hierarchy's, and no user setting is at stake either way.
    "editorConfig.characterAnimation",
    "surfaceConfig.motionConfiguration",
    "surfaceConfig.elementTransitionSpeedScale",
    "surfaceConfig.commandRunnerAnimation",
    "surfaceConfig.uiAnimation",
    "surfaceConfig.editorInsertionTransitionKind",
    "surfaceConfig.commandRunnerTransitionKind",
    "surfaceConfig.panelOpenTransitionKind",
    "surfaceConfig.panelCloseTransitionKind"
  )

  private def isSuperseded(path: String): Boolean =
    supersededByMotionHierarchy.exists(prefix => path == prefix || path.startsWith(s"$prefix."))

  private def differences(path: String, before: Any, after: Any): List[String] =
    (before, after) match
      case (b: Product, a: Product) if b.getClass == a.getClass && b.productArity > 0 =>
        b.productElementNames
          .zip(b.productIterator.zip(a.productIterator))
          .flatMap { case (name, (bv, av)) => differences(if path.isEmpty then name else s"$path.$name", bv, av) }
          .toList
      case (b, a) if b == a || isSuperseded(path) => Nil
      case (b, a)                                 => List(s"$path: saved $b, loaded back $a")

  "a saved config" should "be valid HOCON, whatever the settings are" in {
    noException should be thrownBy ConfigFactory.parseString(savedText(mutated))
    noException should be thrownBy ConfigFactory.parseString(savedText(AppConfig.default))
  }

  /** Fields the round-trip fixture deliberately leaves at their defaults, with the reason. Everything else must be
    * moved off its default by `mutated`, so that adding a setting to [[AppConfig]] and forgetting to persist it fails
    * here rather than silently resetting on the user's next restart.
    */
  private val notExercised: Set[String] = Set(
    // Keyed maps with their own dedicated specs (LspUserConfigSpec, HotkeyConfigSpec, FocusedKeymapConfigSpec) and
    // their own dynamic key prefixes in the schema.
    "languageToolsConfig.lspUserConfig.servers",
    "inputConfig.hotkeyConfig.overrides",
    "inputConfig.focusedKeymapConfig.editor.bindings",
    "inputConfig.focusedKeymapConfig.commandRunner.bindings",
    "inputConfig.focusedKeymapConfig.modal.bindings",
    "inputConfig.focusedKeymapConfig.panel.bindings",
    "inputConfig.focusedKeymapConfig.peek.bindings",
    "inputConfig.hotkeyConfig.bindings"
  )

  private def leafValues(config: AppConfig): Map[String, Any] =
    def walk(path: String, value: Any): List[(String, Any)] =
      value match
        case product: Product if product.productArity > 0 && !product.isInstanceOf[Iterable[?]] =>
          product.productElementNames
            .zip(product.productIterator)
            .flatMap { case (name, element) => walk(if path.isEmpty then name else s"$path.$name", element) }
            .toList
        case other => List(path -> other)
    walk("", config).toMap

  it should "exercise every configurable field, so nothing can be added without being persisted" in {
    val defaults = leafValues(AppConfig.default)
    val changed  = leafValues(mutated)
    val untouched = defaults
      .collect {
        case (path, value) if changed.get(path).contains(value) && !notExercised.contains(path) => path
      }
      .toList
      .sorted

    withClue(
      "these config fields are left at their defaults by this suite's fixture, so nothing checks that they " +
        "survive a save and reload -- move them off their default in `mutated`, or record why not in " +
        s"`notExercised`:\n${untouched.mkString("\n")}\n"
    ) {
      untouched shouldBe empty
    }
  }

  it should "write only keys the format understands" in {
    val unknown = writtenKeys(savedText(mutated)).filterNot(ConfigKeySchema.isKnownKey).toList.sorted
    withClue(s"keys written but not in the schema: ${unknown.mkString(", ")}\n") {
      unknown shouldBe empty
    }
  }

  it should "come back unchanged when every setting is at its default" in {
    val lost = differences("", AppConfig.default, savedAndReloaded(AppConfig.default))
    withClue(s"${lost.size} field(s) lost:\n${lost.mkString("\n")}\n")(lost shouldBe empty)
  }

  it should "come back unchanged when every setting has been moved off its default" in {
    val lost = differences("", mutated, savedAndReloaded(mutated))
    withClue(s"${lost.size} field(s) lost:\n${lost.mkString("\n")}\n")(lost shouldBe empty)
  }

  it should "keep the motion behaviour it saved, which the hierarchy carries rather than the legacy fields" in {
    val reloaded = savedAndReloaded(mutated)

    // Per family, everything except the speed scale, which a saved file deliberately migrates: the writer folds the
    // legacy per-family speed-scale fields into the hierarchy's own, so the reloaded config holds in one place what
    // the in-memory one still holds in two. The four effective accessors below are what behaviour actually reads, and
    // they are what has to agree.
    def families(config: AppConfig): Map[MotionFamily, (Boolean, Any, Any, Any)] =
      config.surfaceConfig.effectiveMotionConfiguration.families.map {
        case (family, settings) =>
          family -> (settings.enabled, settings.transitionKind, settings.animation, settings.transitionOverrides)
      }

    families(reloaded) shouldBe families(mutated)
    reloaded.surfaceConfig.effectiveMotionBaseline shouldBe mutated.surfaceConfig.effectiveMotionBaseline
    reloaded.surfaceConfig.effectiveCommandRunnerTransitionKind shouldBe
      mutated.surfaceConfig.effectiveCommandRunnerTransitionKind
    reloaded.surfaceConfig.effectivePanelOpenTransitionKind shouldBe
      mutated.surfaceConfig.effectivePanelOpenTransitionKind
    reloaded.surfaceConfig.effectiveEditorTextTransitionSpeedScale shouldBe
      mutated.surfaceConfig.effectiveEditorTextTransitionSpeedScale
    reloaded.surfaceConfig.effectiveCommandRunnerTransitionSpeedScale shouldBe
      mutated.surfaceConfig.effectiveCommandRunnerTransitionSpeedScale
    reloaded.surfaceConfig.effectiveUiTransitionSpeedScale shouldBe
      mutated.surfaceConfig.effectiveUiTransitionSpeedScale
    reloaded.surfaceConfig.effectiveCursorTransitionSpeedScale shouldBe
      mutated.surfaceConfig.effectiveCursorTransitionSpeedScale
  }

  it should "survive a cursor info bar with several segments, which needs quoting to stay parseable" in {
    val configured = AppConfig.default
      .withCursorInfoBarSegments(
        List(CursorInfoBarSegment.Position, CursorInfoBarSegment.WordCount, CursorInfoBarSegment.CharCount)
      )
      .withPaneHeaders(false)

    val reloaded = savedAndReloaded(configured)

    reloaded.cursorInfoBarSegments shouldBe configured.cursorInfoBarSegments
    // The bug this pins: the unquoted comma made the whole file unparseable, so every *other* setting reset too.
    reloaded.surfaceConfig.showPaneHeaders shouldBe false
  }

  // -- An unreadable file --------------------------------------------------------------------------------------------

  "a config file that cannot be parsed" should "be kept aside rather than left to be overwritten by defaults" in {
    val file = Files.createTempFile("serenity-unreadable-config", ".conf")
    Files.writeString(file, "display.pane_headers = false\nthis is not = valid = hocon {\n")

    val preserved = ConfigManager.preserveUnreadableConfig(file)

    preserved.map(Files.readString) shouldBe Some(Files.readString(file))
    preserved.map(_.getFileName.toString).exists(_.startsWith(file.getFileName.toString)) shouldBe true
    preserved.foreach(Files.deleteIfExists(_): Unit)
    Files.deleteIfExists(file): Unit
  }

  it should "report itself as a load failure rather than quietly returning defaults" in {
    val file = Files.createTempFile("serenity-unreadable-config", ".conf")
    // What older versions wrote: a quoted key (so the legacy line-based reader declines the file) alongside the
    // unquoted comma that makes it invalid HOCON. Every setting in such a file was silently replaced by defaults.
    Files.writeString(
      file,
      """"character.animation" = none
        |"cursor.info_bar" = position,word_count
        |cursor.info_bar.placement = floating
        |""".stripMargin
    )

    val outcome = ConfigManager.loadConfigResultIO(Some(file.toString)).unsafeRunSync()

    outcome.isLeft shouldBe true
    Files.deleteIfExists(file): Unit
  }
