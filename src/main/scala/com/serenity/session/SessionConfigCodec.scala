package com.serenity.session

import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.config.{
  AppConfig,
  ConfigRegistry,
  CursorColorConfig,
  CursorInfoBarSegment,
  FocusedKeymapConfig,
  HotkeyConfig,
  MotionConfig,
  MotionPreset,
  PreferredWindowSize,
  SpellCheckConfig,
  TextAreaInsets
}
import com.serenity.lsp.config.LspUserConfig
import com.serenity.ui.fonts.FontLoader.FontConfig
import io.circe.{Decoder, Encoder, HCursor, Json}

/** A setting session state keeps whole where the config file spreads it over several keys.
  *
  * These have no single config key, so they are not [[ConfigField]]s -- but they are still settings that have to
  * survive a restore, and the coverage test counts them alongside the registry's.
  */
final case class SessionField[A](key: String, get: AppConfig => A, set: (AppConfig, A) => AppConfig)(using
    encoder: Encoder[A],
    decoder: Decoder[A]
):
  def encode(config: AppConfig): (String, Json) = key -> encoder(get(config))

  /** A key the file does not carry is left alone: `Option` decodes a missing key as `None`, which would turn every
    * default into "not set" on an older session file.
    */
  def decode(cursor: HCursor, config: AppConfig): AppConfig =
    val stored = cursor.downField(key)
    if !stored.succeeded then config
    else stored.as(using decoder).fold(_ => config, value => set(config, value))

/** How an `AppConfig` is written into a session file and read back out.
  *
  * Everything written comes from [[ConfigRegistry]] plus [[composites]], so a setting cannot be saved to the config
  * file and dropped from session state -- which is what had happened to sixteen of them, `showPaneHeaders` and the
  * viewport margins among them. Because the restored session's config replaces the running one wholesale, those
  * settings went back to their defaults on every restore no matter what the config file said.
  *
  * The composites set `SurfaceConfig` fields directly rather than through `AppConfig`'s own setters: a restore should
  * put back exactly what was saved, and several of those setters deliberately adjust neighbouring settings.
  */
object SessionConfigCodec:

  val composites: List[SessionField[?]] = List(
    SessionField[Option[AnimationConfig]](
      "characterAnimation",
      _.editorConfig.characterAnimation,
      (config, value) => config.withEditorConfig(config.editorConfig.copy(characterAnimation = value))
    ),
    SessionField[HotkeyConfig](
      "hotkeyConfig",
      _.inputConfig.hotkeyConfig,
      (config, value) => config.withInputConfig(config.inputConfig.copy(hotkeyConfig = value))
    ),
    SessionField[FocusedKeymapConfig](
      "focusedKeymapConfig",
      _.inputConfig.focusedKeymapConfig,
      (config, value) => config.withInputConfig(config.inputConfig.copy(focusedKeymapConfig = value))
    ),
    SessionField[LspUserConfig](
      "lspUserConfig",
      _.languageToolsConfig.lspUserConfig,
      (config, value) => config.withLanguageToolsConfig(config.languageToolsConfig.copy(lspUserConfig = value))
    ),
    SessionField[MotionPreset](
      "motionPreset",
      _.surfaceConfig.motionPreset,
      (config, value) => config.withSurfaceConfig(config.surfaceConfig.copy(motionPreset = value))
    ),
    SessionField[Double](
      "elementTransitionSpeedScale",
      _.surfaceConfig.elementTransitionSpeedScale,
      (config, value) =>
        config.withSurfaceConfig(
          config.surfaceConfig.copy(elementTransitionSpeedScale = AppConfig.clampElementTransitionSpeedScale(value))
        )
    ),
    SessionField[Option[Double]](
      "editorTextTransitionSpeedScale",
      _.surfaceConfig.editorTextTransitionSpeedScale,
      (config, value) =>
        config.withSurfaceConfig(
          config.surfaceConfig
            .copy(editorTextTransitionSpeedScale = value.map(AppConfig.clampElementTransitionSpeedScale))
        )
    ),
    SessionField[Option[Double]](
      "commandRunnerTransitionSpeedScale",
      _.surfaceConfig.commandRunnerTransitionSpeedScale,
      (config, value) =>
        config.withSurfaceConfig(
          config.surfaceConfig
            .copy(commandRunnerTransitionSpeedScale = value.map(AppConfig.clampElementTransitionSpeedScale))
        )
    ),
    SessionField[Option[Double]](
      "uiTransitionSpeedScale",
      _.surfaceConfig.uiTransitionSpeedScale,
      (config, value) =>
        config.withSurfaceConfig(
          config.surfaceConfig.copy(uiTransitionSpeedScale = value.map(AppConfig.clampElementTransitionSpeedScale))
        )
    ),
    SessionField[Option[Double]](
      "cursorTransitionSpeedScale",
      _.surfaceConfig.cursorTransitionSpeedScale,
      (config, value) =>
        config.withSurfaceConfig(
          config.surfaceConfig.copy(cursorTransitionSpeedScale = value.map(AppConfig.clampElementTransitionSpeedScale))
        )
    ),
    SessionField[Option[AnimationConfig]](
      "commandRunnerAnimation",
      _.surfaceConfig.commandRunnerAnimation,
      (config, value) => config.withSurfaceConfig(config.surfaceConfig.copy(commandRunnerAnimation = value))
    ),
    SessionField[Option[AnimationConfig]](
      "uiAnimation",
      _.surfaceConfig.uiAnimation,
      (config, value) => config.withSurfaceConfig(config.surfaceConfig.copy(uiAnimation = value))
    ),
    SessionField[TransitionKind](
      "editorInsertionTransitionKind",
      _.surfaceConfig.editorInsertionTransitionKind,
      (config, value) => config.withSurfaceConfig(config.surfaceConfig.copy(editorInsertionTransitionKind = value))
    ),
    SessionField[Option[TransitionKind]](
      "commandRunnerTransitionKind",
      _.surfaceConfig.commandRunnerTransitionKind,
      (config, value) => config.withSurfaceConfig(config.surfaceConfig.copy(commandRunnerTransitionKind = value))
    ),
    SessionField[Option[TransitionKind]](
      "panelOpenTransitionKind",
      _.surfaceConfig.panelOpenTransitionKind,
      (config, value) => config.withSurfaceConfig(config.surfaceConfig.copy(panelOpenTransitionKind = value))
    ),
    SessionField[Option[TransitionKind]](
      "panelCloseTransitionKind",
      _.surfaceConfig.panelCloseTransitionKind,
      (config, value) => config.withSurfaceConfig(config.surfaceConfig.copy(panelCloseTransitionKind = value))
    ),
    SessionField[Option[MotionConfig]](
      "motionConfiguration",
      _.surfaceConfig.motionConfiguration,
      (config, value) => config.withSurfaceConfig(config.surfaceConfig.copy(motionConfiguration = value))
    ),
    SessionField[FontConfig](
      "fontConfig",
      _.editorConfig.fontConfig,
      (config, value) => config.withFontConfig(value)
    ),
    SessionField[SpellCheckConfig](
      "spellCheck",
      _.languageToolsConfig.spellCheck,
      (config, value) => config.withSpellCheck(value)
    ),
    SessionField[CursorColorConfig](
      "cursorColors",
      _.cursorColors,
      (config, value) => config.withCursorColors(value)
    ),
    SessionField[TextAreaInsets](
      "textAreaInsets",
      _.surfaceConfig.textAreaInsets,
      (config, value) => config.withTextAreaInsets(value)
    ),
    SessionField[Option[PreferredWindowSize]](
      "preferredWindowSize",
      _.preferredWindowSize,
      (config, value) => config.withWindowConfig(config.windowConfig.copy(preferredSize = value.map(_.normalized)))
    )
  )

  /** Settings one of the [[composites]] already carries whole.
    *
    * The config file spreads these over a key each -- a font family, an inset percentage -- but session files have
    * always kept the object, and it round-trips completely. Writing both would put the same setting in twice and leave
    * the reader to pick.
    */
  private val carriedByComposite: Set[String] =
    Set(
      "font.code.family",
      "font.text.family",
      "font.ui.family",
      "font.code.size",
      "font.text.size",
      "font.ui.size",
      "font.scale.mode",
      "font.text_scale",
      "font.code.ligatures",
      "font.text.ligatures",
      "font.ui.ligatures",
      "spellcheck.enabled",
      "spellcheck.languages",
      "spellcheck.dictionary_paths",
      "spellcheck.words",
      "cursor.active.color",
      "cursor.inactive.color",
      "text_area.left.percent",
      "text_area.right.percent",
      "text_area.top.percent",
      "text_area.bottom.percent",
      "window.preferred.width",
      "window.preferred.height"
    )

  private val standaloneFields: List[com.serenity.config.ConfigField[?]] =
    ConfigRegistry.fields.filterNot(field => carriedByComposite.contains(field.key))

  private val restoreOrder: List[com.serenity.config.ConfigField[?]] =
    ConfigRegistry.readOrder.filterNot(field => carriedByComposite.contains(field.key))

  def encode(config: AppConfig): Json =
    Json.obj((standaloneFields.map(_.encode(config)) ++ composites.map(_.encode(config)))*)

  /** Read the whole config back, oldest shapes first so anything the current form also carries still wins.
    *
    * Registry fields are applied in [[ConfigRegistry.readOrder]] -- the same broader-before-narrower order the config
    * file is folded in -- because a few setters deliberately adjust a neighbouring setting, and the order decides
    * whether the neighbour's own saved value gets the last word.
    */
  def decode(cursor: HCursor): AppConfig =
    val withFields    = restoreOrder.foldLeft(AppConfig.default)((config, field) => field.decode(cursor, config))
    val withComposite = composites.foldLeft(withFields)((config, field) => field.decode(cursor, config))
    readSupersededShapes(cursor, withComposite)

  /** The one shape an older session file used that nothing reads any more.
    *
    * Sessions before #1261 stored a single `cursorInfoBarMode` string instead of an ordered segment list. The old
    * "detailed" preset (position + language + filename) has no exact equivalent since language was dropped from the
    * segment set, so it maps to the closest available pair.
    */
  private def readSupersededShapes(cursor: HCursor, config: AppConfig): AppConfig =
    if cursor.downField("cursorInfoBarSegments").succeeded then config
    else
      cursor
        .downField("cursorInfoBarMode")
        .as[String]
        .toOption
        .flatMap(legacyCursorInfoBarMode)
        .fold(config)(config.withCursorInfoBarSegments)

  private def legacyCursorInfoBarMode(value: String): Option[List[CursorInfoBarSegment]] =
    value.trim.toLowerCase match
      case "off" | "false" | "disabled" => Some(Nil)
      case "position" | "minimal"       => Some(List(CursorInfoBarSegment.Position))
      case "detailed" | "full"          => Some(List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title))
      case _                            => None
