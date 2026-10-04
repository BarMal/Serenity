package com.serenity.testkit

import java.awt.Color

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
      fonts     <- genFontConfig
      paneWidth <- Gen.choose(1, 200)
    yield EditorConfig(fontConfig = fonts, minimumPaneWidth = paneWidth)

  val genSpellCheckConfig: Gen[SpellCheckConfig] =
    for
      enabled   <- Gen.oneOf(true, false)
      languages <- Gen.nonEmptyListOf(Gen.oneOf("en", "fr", "de", "en-gb")).map(_.distinct)
      paths     <- Gen.listOf(Gen.oneOf("/tmp/words.dic", "/usr/share/dict/words")).map(_.distinct)
      words     <- Gen.listOf(Gen.alphaStr.suchThat(_.nonEmpty)).map(_.distinct)
    yield SpellCheckConfig(enabled, languages, paths, words).normalized

  val genCursorConfig: Gen[CursorConfig] =
    for
      mode     <- oneOfEnum(CursorMode.values)
      active   <- Gen.option(genColor)
      inactive <- Gen.option(genColor)
      timeout  <- Gen.chooseNum(0L, 60000L)
    yield CursorConfig(mode, CursorColorConfig(active, inactive), timeout)

  val genStatusLineConfig: Gen[StatusLineConfig] =
    for
      segments   <- Gen.someOf(StatusSegment.values.toIndexedSeq).map(_.toList)
      placement  <- oneOfEnum(StatusLinePlacement.values)
      foreground <- Gen.option(genColor)
      background <- Gen.option(genColor)
      alpha      <- Gen.option(double(0.0, 1.0))
    yield StatusLineConfig(segments, placement, StatusLineColors(foreground, background, alpha))

  val genWindowConfig: Gen[WindowConfig] =
    for
      chrome <- oneOfEnum(WindowChromeMode.values)
      // Above `PreferredWindowSize.normalized`'s own floor, so the generated value is one the application would keep.
      size <- Gen.option(for w <- Gen.choose(400, 4000); h <- Gen.choose(300, 4000) yield PreferredWindowSize(w, h))
    yield WindowConfig(chrome, size)

  val genDocumentConfig: Gen[DocumentConfig] =
    for
      markdown <- oneOfEnum(MarkdownViewMode.values)
      default  <- oneOfEnum(DefaultDocumentMode.values)
      goal     <- Gen.option(Gen.choose(1, 100000))
      dropCaps <- Gen.oneOf(true, false)
    yield DocumentConfig(markdown, default, goal, dropCaps)

  val genAppModeConfig: Gen[AppModeConfig] =
    for
      mode    <- oneOfEnum(AppMode.values)
      showAll <- Gen.oneOf(true, false)
    yield AppModeConfig(mode, showAll)

  val genInterfaceConfig: Gen[InterfaceConfig] =
    for
      density   <- oneOfEnum(InterfaceDensity.values)
      gap       <- Gen.option(double(AppConfig.MinUiElementGap, AppConfig.MaxUiElementGap))
      thickness <- Gen.choose(AppConfig.MinUiOutlineThicknessPx, AppConfig.MaxUiOutlineThicknessPx)
    yield InterfaceConfig(density, gap, thickness)

  val genInputConfig: Gen[InputConfig] =
    for
      lines       <- Gen.choose(1, 50)
      codeEscape  <- oneOfEnum(PanelEscapeTarget.values)
      proseEscape <- oneOfEnum(PanelEscapeTarget.values)
    yield InputConfig(wheelScrollLines = lines, panelEscapeReturnsTo = PerMode(codeEscape, proseEscape))

  val genTextAreaInsets: Gen[TextAreaInsets] =
    for
      left   <- double(0.0, 0.4)
      right  <- double(0.0, 0.4)
      top    <- double(0.0, 0.4)
      bottom <- double(0.0, 0.4)
    yield TextAreaInsets(left, right, top, bottom).normalized

  val genLineNumberLayout: Gen[LineNumberLayout] =
    for
      side        <- oneOfEnum(LineNumberSide.values)
      marginLeft  <- Gen.option(Gen.choose(0, LineNumberLayout.MaxCells))
      marginRight <- Gen.choose(0, LineNumberLayout.MaxCells)
      padding     <- Gen.option(Gen.choose(0, LineNumberLayout.MaxCells))
    yield LineNumberLayout(side, marginLeft, marginRight, padding)

  val genViewportAxisSizing: Gen[ViewportAxisSizing] =
    for
      percent <- double(ViewportAxisSizing.MinPercent, ViewportAxisSizing.MaxPercent)
      max     <- Gen.option(Gen.choose(1, 500))
    yield ViewportAxisSizing(percent, max)

  /** Surface settings that are independent of one another, constructed directly. */
  val genSurfaceConfig: Gen[SurfaceConfig] =
    for
      lineNumbers         <- Gen.oneOf(true, false)
      paneHeaders         <- Gen.oneOf(true, false)
      comments            <- oneOfEnum(CommentDisplayMode.values)
      wordWrap            <- Gen.oneOf(true, false)
      visualLineNav       <- Gen.oneOf(true, false)
      typewriterScrolling <- Gen.oneOf(true, false)
      focusedTextBody     <- Gen.oneOf(true, false)
      toolbar             <- Gen.oneOf(true, false)
      toolbarMode         <- oneOfEnum(ToolbarDisplayMode.values)
      visibleRows <- Gen.option(
        Gen.choose(AppConfig.MinCommandRunnerVisibleRows, AppConfig.MaxCommandRunnerVisibleRows)
      )
      itemGap <- Gen.option(
        double(AppConfig.MinCommandRunnerItemGapRows, AppConfig.MaxCommandRunnerItemGapRows)
      )
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
      // Above/below the cursor only (issue #1310: `SurfacePlacement.Corner` isn't a valid value for this cursor-peek
      // setting, and being a parameterized case, it also means `.values` is no longer generated for the enum).
      peekPlacement    <- oneOfEnum(Array(SurfacePlacement.AboveCursor, SurfacePlacement.BelowCursor))
      fpsTarget        <- oneOfEnum(RenderFpsTarget.values)
      damage           <- oneOfEnum(RenderDamageGranularity.values)
      insets           <- genTextAreaInsets
      lineNumberLayout <- genLineNumberLayout
      width            <- genViewportAxisSizing
      height           <- genViewportAxisSizing
      frameStateCacheCapacity <- Gen.choose(
        AppConfig.MinRendererFrameStateCacheCapacity,
        AppConfig.MaxRendererFrameStateCacheCapacity
      )
      layerCaching <- Gen.oneOf(true, false)
      frameTiming  <- Gen.oneOf(true, false)
      latencyTrace <- Gen.oneOf(true, false)
      warmUp       <- Gen.oneOf(true, false)
      diagnosticBlendWeight <- double(
        AppConfig.MinDiagnosticHighlightBlendWeight,
        AppConfig.MaxDiagnosticHighlightBlendWeight
      )
      columnMode        <- Gen.oneOf(true, false)
      columnTargetWidth <- Gen.choose(1, 400)
      columnGap         <- Gen.choose(0, 40)
      columnCount       <- Gen.option(Gen.choose(1, 20))
    yield SurfaceConfig(
      showLineNumbers = lineNumbers,
      showPaneHeaders = paneHeaders,
      commentDisplayMode = comments,
      wordWrapEnabled = wordWrap,
      visualLineCursorNavigation = visualLineNav,
      typewriterScrollingEnabled = typewriterScrolling,
      focusedTextBodyEnabled = focusedTextBody,
      contextualToolbarEnabled = toolbar,
      contextualToolbarDisplayMode = toolbarMode,
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
      lineNumberLayout = lineNumberLayout,
      viewportSizing = ViewportSizing(width, height),
      rendererFrameStateCacheCapacity = frameStateCacheCapacity,
      layerCachingEnabled = layerCaching,
      frameTimingEnabled = frameTiming,
      latencyTraceEnabled = latencyTrace,
      startupWarmUpEnabled = warmUp,
      diagnosticHighlightBlendWeight = diagnosticBlendWeight,
      columnModeEnabled = columnMode,
      columnTargetWidthCells = columnTargetWidth,
      columnGap = columnGap,
      columnCount = columnCount
    )

  val genAppConfig: Gen[AppConfig] =
    for
      editor           <- genEditorConfig
      surface          <- genSurfaceConfig
      cursor           <- genCursorConfig
      window           <- genWindowConfig
      document         <- genDocumentConfig
      interface        <- genInterfaceConfig
      input            <- genInputConfig
      syntax           <- Gen.oneOf(true, false)
      smartPunctuation <- Gen.oneOf(true, false)
      spell            <- genSpellCheckConfig
      appMode          <- genAppModeConfig
      status           <- genStatusLineConfig
    yield AppConfig(
      editorConfig = editor,
      inputConfig = input,
      surfaceConfig = surface,
      cursorConfig = cursor,
      windowConfig = window,
      documentConfig = document,
      interfaceConfig = interface,
      languageToolsConfig = LanguageToolsConfig(
        syntaxHighlightingEnabled = syntax,
        spellCheck = spell,
        smartPunctuationEnabled = smartPunctuation
      ),
      appModeConfig = appMode,
      statusLine = status
    )
