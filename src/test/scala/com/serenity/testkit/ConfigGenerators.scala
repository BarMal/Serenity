package com.serenity.testkit

import java.awt.Color

import scala.concurrent.duration.DurationInt

import com.serenity.animation.{AnimationConfig, TransitionKind, TransitionScope, WindowSitterAction, WindowSitterConfig}
import com.serenity.config.*
import com.serenity.keystroke.Modifier
import com.serenity.state.models.SurfacePlacement
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import org.scalacheck.Gen

/** Generators over [[AppConfig]], for properties about the config file format.
  *
  * Values are drawn inside their own clamps (`AppConfig.clamp*`, `SurfaceConfig.normalized`, ...) so that `normalized`
  * is the identity on anything generated here. That keeps the round-trip property a plain equality --
  * `decode(encode(c)) == c` -- rather than an equality modulo a normalisation step, which would quietly excuse a codec
  * that lost precision or clamped on the way through.
  *
  * The keyed maps (hotkeys, focused keymaps, LSP server overrides) are left at their defaults: they have their own
  * codecs, their own dynamic key prefixes in `ConfigKeySchema`, and their own specs, and generating conflicting
  * bindings would test those rather than the settings format.
  */
object ConfigGenerators:

  private def double(min: Double, max: Double): Gen[Double] =
    Gen.choose(min, max).map(value => BigDecimal(value).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble)

  private def oneOfEnum[A](values: Array[A]): Gen[A] = Gen.oneOf(values.toIndexedSeq)

  val genColor: Gen[Color] =
    for
      red   <- Gen.choose(0, 255)
      green <- Gen.choose(0, 255)
      blue  <- Gen.choose(0, 255)
      alpha <- Gen.choose(0, 255)
    yield Color(red, green, blue, alpha)

  /** Font families are written as plain strings, so they are where a quoting fault in the writer would show first:
    * spaces, quotes, backslashes and HOCON's own comment and substitution markers all have to survive.
    */
  val genFontFamily: Gen[String] =
    Gen.oneOf(
      Gen.const("Monaspace Neon"),
      Gen.const("JetBrains Mono, Nerd Font"),
      Gen.const("""Font "With" Quotes"""),
      Gen.const("""Back\slash"""),
      Gen.const("# not a comment"),
      Gen.const("${not.a.substitution}"),
      Gen.const("Ünïcödé Serif"),
      Gen.alphaStr.suchThat(_.nonEmpty)
    )

  val genAnimationConfig: Gen[AnimationConfig] =
    for
      steps      <- Gen.choose(1, 60)
      durationMs <- Gen.choose(1, 5000)
    yield AnimationConfig(steps, durationMs.milliseconds)

  val genFontConfig: Gen[FontConfig] =
    for
      codeFamily <- genFontFamily
      textFamily <- genFontFamily
      uiFamily   <- genFontFamily
      codeSize   <- double(8.0, 48.0).map(_.toFloat)
      textSize   <- double(8.0, 48.0).map(_.toFloat)
      uiSize     <- double(8.0, 48.0).map(_.toFloat)
      scaleMode  <- oneOfEnum(FontLoader.TextScaleMode.values)
      // The multiplier only means anything in manual mode: `FontConfig.resolveAutoTextScale` derives it from the
      // display under `Auto` and pins it to 1.0 under `Off`, so any other pairing describes a config the application
      // never holds.
      chosenMultiplier <- double(0.5, 3.0)
      multiplier = if scaleMode == FontLoader.TextScaleMode.Manual then chosenMultiplier else 1.0
      codeLigs <- Gen.oneOf(true, false)
      textLigs <- Gen.oneOf(true, false)
      uiLigs   <- Gen.oneOf(true, false)
    yield FontConfig(
      codeFontFamily = codeFamily,
      textFontFamily = textFamily,
      uiFontFamily = uiFamily,
      fontSize = codeSize,
      textFontSize = textSize,
      uiFontSize = uiSize,
      textScaleMode = scaleMode,
      textScaleMultiplier = multiplier,
      enableLigatures = codeLigs,
      textLigatures = textLigs,
      uiLigatures = uiLigs
    )

  val genEditorConfig: Gen[EditorConfig] =
    for
      animation <- Gen.option(genAnimationConfig)
      fonts     <- genFontConfig
      paneWidth <- Gen.choose(1, 200)
    yield EditorConfig(characterAnimation = animation, fontConfig = fonts, minimumPaneWidth = paneWidth)

  val genSpellCheckConfig: Gen[SpellCheckConfig] =
    for
      enabled   <- Gen.oneOf(true, false)
      languages <- Gen.nonEmptyListOf(Gen.oneOf("en", "fr", "de", "en-gb")).map(_.distinct)
      paths     <- Gen.listOf(Gen.oneOf("/tmp/words.dic", "/usr/share/dict/words")).map(_.distinct)
      words     <- Gen.listOf(Gen.alphaStr.suchThat(_.nonEmpty)).map(_.distinct)
    yield SpellCheckConfig(enabled, languages, paths, words).normalized

  val genCursorConfig: Gen[CursorConfig] =
    for
      mode      <- oneOfEnum(CursorMode.values)
      active    <- Gen.option(genColor)
      inactive  <- Gen.option(genColor)
      segments  <- Gen.someOf(CursorInfoBarSegment.values.toIndexedSeq).map(_.toList)
      placement <- oneOfEnum(CursorInfoBarPlacement.values)
    yield CursorConfig(mode, CursorColorConfig(active, inactive), segments, placement)

  val genWindowConfig: Gen[WindowConfig] =
    for
      chrome <- oneOfEnum(WindowChromeMode.values)
      // Above `PreferredWindowSize.normalized`'s own floor, so the generated value is one the application would keep.
      size <- Gen.option(for w <- Gen.choose(400, 4000); h <- Gen.choose(300, 4000) yield PreferredWindowSize(w, h))
    yield WindowConfig(chrome, size)

  val genWindowSitterConfig: Gen[WindowSitterConfig] =
    for
      enabled <- Gen.oneOf(true, false)
      action  <- oneOfEnum(WindowSitterAction.values)
      // `WindowSitterConfig.normalized` keeps at most 32 frames; `·` is in the shipped default, and is the character
      // that showed the config file was being read back with the platform charset rather than UTF-8.
      frames    <- Gen.nonEmptyListOf(Gen.oneOf("-", "+", "o", "O", "·")).map(_.take(32).toVector)
      ticks     <- Gen.choose(1, 120)
      fastTicks <- Gen.choose(1, 240)
      threshold <- Gen.choose(1, 5000)
    yield WindowSitterConfig(enabled, action, frames, ticks, fastTicks, threshold)

  val genDocumentConfig: Gen[DocumentConfig] =
    for
      markdown <- oneOfEnum(MarkdownViewMode.values)
      default  <- oneOfEnum(DefaultDocumentMode.values)
    yield DocumentConfig(markdown, default)

  val genInterfaceConfig: Gen[InterfaceConfig] =
    for
      density   <- oneOfEnum(InterfaceDensity.values)
      gap       <- double(AppConfig.MinUiElementGap, AppConfig.MaxUiElementGap)
      radius    <- Gen.choose(AppConfig.MinUiCornerRadiusPx, AppConfig.MaxUiCornerRadiusPx)
      thickness <- Gen.choose(AppConfig.MinUiOutlineThicknessPx, AppConfig.MaxUiOutlineThicknessPx)
    yield InterfaceConfig(density, gap, radius, thickness)

  val genInputConfig: Gen[InputConfig] =
    Gen.choose(1, 50).map(lines => InputConfig(wheelScrollLines = lines))

  val genTextAreaInsets: Gen[TextAreaInsets] =
    for
      left   <- double(0.0, 0.4)
      right  <- double(0.0, 0.4)
      top    <- double(0.0, 0.4)
      bottom <- double(0.0, 0.4)
    yield TextAreaInsets(left, right, top, bottom).normalized

  val genViewportAxisSizing: Gen[ViewportAxisSizing] =
    for
      percent <- double(ViewportAxisSizing.MinPercent, ViewportAxisSizing.MaxPercent)
      max     <- Gen.option(Gen.choose(1, 500))
    yield ViewportAxisSizing(percent, max)

  val genMotionFamilyConfig: Gen[MotionFamilyConfig] =
    for
      enabled    <- Gen.oneOf(true, false)
      transition <- oneOfEnum(TransitionKind.values)
      animation  <- Gen.option(genAnimationConfig)
      speed      <- double(AppConfig.MinElementTransitionSpeedScale, AppConfig.MaxElementTransitionSpeedScale)
    yield MotionFamilyConfig(enabled, transition, animation, speed)

  val genMotionConfig: Gen[MotionConfig] =
    for
      accessibility <- oneOfEnum(MotionAccessibility.values)
      baseline      <- oneOfEnum(MotionPreset.values)
      families <- Gen.sequence[List[(MotionFamily, MotionFamilyConfig)], (MotionFamily, MotionFamilyConfig)](
        MotionFamily.values.toList.map(family => genMotionFamilyConfig.map(family -> _))
      )
      panelOpen  <- oneOfEnum(TransitionKind.values)
      panelClose <- oneOfEnum(TransitionKind.values)
    yield
      val withOverrides = families.map {
        case (MotionFamily.PinnedPanels, settings) =>
          MotionFamily.PinnedPanels -> settings.copy(transitionOverrides =
            Map(TransitionScope.PanelOpen -> panelOpen, TransitionScope.PanelClose -> panelClose)
          )
        case other => other
      }
      MotionConfig(accessibility, baseline, withOverrides.toMap)

  /** Surface settings that are independent of one another, constructed directly.
    *
    * The motion settings are deliberately *not* here: `motionConfiguration` and the legacy fields it supersedes
    * (`motionPreset`, the transition kinds, the animations, the speed scales) are kept in step by `AppConfig`'s own
    * setters, so setting them independently would describe a config the application can never be in -- and a round-trip
    * property over unreachable states tests the generator, not the format. They are applied through those setters in
    * [[genAppConfig]] instead.
    */
  val genSurfaceConfig: Gen[SurfaceConfig] =
    for
      lineNumbers     <- Gen.oneOf(true, false)
      gutter          <- Gen.oneOf(true, false)
      paneHeaders     <- Gen.oneOf(true, false)
      wordCount       <- Gen.oneOf(true, false)
      comments        <- oneOfEnum(CommentDisplayMode.values)
      wordWrap        <- Gen.oneOf(true, false)
      visualLineNav   <- Gen.oneOf(true, false)
      focusedTextBody <- Gen.oneOf(true, false)
      toolbar         <- Gen.oneOf(true, false)
      toolbarMode     <- oneOfEnum(ToolbarDisplayMode.values)
      postProcessing  <- oneOfEnum(PostProcessingEffect.values)
      shadows         <- Gen.oneOf(true, false)
      visibleRows <- Gen.option(
        Gen.choose(AppConfig.MinCommandRunnerVisibleRows, AppConfig.MaxCommandRunnerVisibleRows)
      )
      itemGap <- double(AppConfig.MinCommandRunnerItemGapRows, AppConfig.MaxCommandRunnerItemGapRows)
      cursorGap <- Gen.option(
        double(AppConfig.MinCommandRunnerCursorGapRows, AppConfig.MaxCommandRunnerCursorGapRows)
      )
      keyHints     <- Gen.oneOf(true, false)
      peekEnabled  <- Gen.oneOf(true, false)
      peekModifier <- oneOfEnum(Modifier.values)
      peekTapWindow <- Gen.choose(
        AppConfig.MinCommandRunnerCursorPeekTapWindowMillis,
        AppConfig.MaxCommandRunnerCursorPeekTapWindowMillis
      )
      peekPlacement <- oneOfEnum(SurfacePlacement.values)
      fpsTarget     <- oneOfEnum(RenderFpsTarget.values)
      damage        <- oneOfEnum(RenderDamageGranularity.values)
      insets        <- genTextAreaInsets
      width         <- genViewportAxisSizing
      height        <- genViewportAxisSizing
      infoBarAlpha  <- Gen.option(double(0.0, 1.0))
    yield SurfaceConfig(
      showLineNumbers = lineNumbers,
      showGutter = gutter,
      showPaneHeaders = paneHeaders,
      showWordCount = wordCount,
      commentDisplayMode = comments,
      wordWrapEnabled = wordWrap,
      visualLineCursorNavigation = visualLineNav,
      focusedTextBodyEnabled = focusedTextBody,
      contextualToolbarEnabled = toolbar,
      contextualToolbarDisplayMode = toolbarMode,
      postProcessingEffect = postProcessing,
      uiShadowsEnabled = shadows,
      commandRunnerVisibleRows = visibleRows,
      commandRunnerItemGapRows = itemGap,
      commandRunnerCursorGapRows = cursorGap,
      commandRunnerShowKeyHints = keyHints,
      commandRunnerCursorPeekEnabled = peekEnabled,
      commandRunnerCursorPeekModifier = peekModifier,
      commandRunnerCursorPeekTapWindowMillis = peekTapWindow,
      commandRunnerCursorPeekPlacement = peekPlacement,
      renderFpsTarget = fpsTarget,
      renderDamageGranularity = damage,
      textAreaInsets = insets,
      viewportSizing = ViewportSizing(width, height),
      cursorInfoBarBackgroundAlpha = infoBarAlpha
    )

  /** The material settings, applied through the setters for the same reason as the motion ones: choosing a blur or a
    * background style of your own is what makes the material preset `Custom`, so setting them independently of the
    * preset describes a config the application never produces.
    */
  val genMaterialEdit: Gen[AppConfig => AppConfig] =
    for
      preset     <- oneOfEnum(MaterialPreset.values)
      blur       <- double(0.0, 1.0).map(_.toFloat)
      background <- oneOfEnum(BackgroundStyle.values)
      custom     <- Gen.oneOf(true, false)
    yield (config: AppConfig) =>
      val withPreset = config.withMaterialPreset(preset)
      if custom then withPreset.withBlurRadius(blur).withBackgroundStyle(background) else withPreset

  /** The motion settings, applied the way the settings surface applies them: through `AppConfig`'s setters, which keep
    * the authoritative hierarchy and the legacy fields that mirror it in step.
    */
  val genMotionEdit: Gen[AppConfig => AppConfig] =
    for
      preset         <- oneOfEnum(MotionPreset.values.filterNot(_ == MotionPreset.Custom))
      accessibility  <- oneOfEnum(MotionAccessibility.values)
      family         <- oneOfEnum(MotionFamily.values)
      settings       <- genMotionFamilyConfig
      elementSpeed   <- double(AppConfig.MinElementTransitionSpeedScale, AppConfig.MaxElementTransitionSpeedScale)
      insertion      <- oneOfEnum(TransitionKind.values)
      runnerKind     <- Gen.option(oneOfEnum(TransitionKind.values))
      panelOpenKind  <- Gen.option(oneOfEnum(TransitionKind.values))
      panelCloseKind <- Gen.option(oneOfEnum(TransitionKind.values))
      editorSpeed    <- Gen.option(double(0.1, 4.0))
      runnerSpeed    <- Gen.option(double(0.1, 4.0))
      uiSpeed        <- Gen.option(double(0.1, 4.0))
      cursorSpeed    <- Gen.option(double(0.1, 4.0))
    yield (config: AppConfig) =>
      // Preset first, then the finer settings -- the order the settings surface applies them in. The other way round,
      // `withElementTransitionSpeedScale` writes only its legacy field (`updateAuthoritativeMotion` propagates into the
      // hierarchy only when one already exists), and the preset then installs a hierarchy that does not carry it: a
      // config whose effective speed and whose hierarchy disagree, which is not a state worth holding the file format
      // to.
      config
        .withMotionPreset(preset)
        .withMotionAccessibility(accessibility)
        .withElementTransitionSpeedScale(elementSpeed)
        .withEditorInsertionTransitionKind(insertion)
        .withCommandRunnerTransitionKind(runnerKind)
        .withPanelOpenTransitionKind(panelOpenKind)
        .withPanelCloseTransitionKind(panelCloseKind)
        .withMotionFamilyConfiguration(family, settings)
        .withEditorTextTransitionSpeedScale(editorSpeed)
        .withCommandRunnerTransitionSpeedScale(runnerSpeed)
        .withUiTransitionSpeedScale(uiSpeed)
        .withCursorTransitionSpeedScale(cursorSpeed)

  val genAppConfig: Gen[AppConfig] =
    for
      editor    <- genEditorConfig
      surface   <- genSurfaceConfig
      cursor    <- genCursorConfig
      window    <- genWindowConfig
      sitter    <- genWindowSitterConfig
      document  <- genDocumentConfig
      interface <- genInterfaceConfig
      input     <- genInputConfig
      syntax    <- Gen.oneOf(true, false)
      spell     <- genSpellCheckConfig
      motion    <- genMotionEdit
      material  <- genMaterialEdit
    yield (motion andThen material)(
      AppConfig(
        editorConfig = editor,
        inputConfig = input,
        surfaceConfig = surface,
        cursorConfig = cursor,
        windowConfig = window,
        windowSitterConfig = sitter,
        documentConfig = document,
        interfaceConfig = interface,
        languageToolsConfig = LanguageToolsConfig(syntaxHighlightingEnabled = syntax, spellCheck = spell)
      )
    )
