package com.serenity.ui.fonts

import java.awt.image.BufferedImage
import java.awt.{Font, GraphicsEnvironment}

import cats.effect.IO
import com.serenity.state.models.TypographyRole
import com.serenity.ui.layout.CellMetrics
import org.typelevel.log4cats.Logger

object FontLoader:

  val BundledCodeFontFamily = "Monaspace Neon (Bundled)"
  val ToolbarIconFontFamily = "Material Icons Round"

  enum TextScaleMode(val configKey: String):
    case Auto   extends TextScaleMode("auto")
    case Manual extends TextScaleMode("manual")
    case Off    extends TextScaleMode("off")

  final case class FontConfig(
      codeFontFamily: String = BundledCodeFontFamily,
      textFontFamily: String = Font.SANS_SERIF,
      uiFontFamily: String = Font.SANS_SERIF,
      fontSize: Float = 12.0f,
      textFontSize: Float = 12.0f,
      uiFontSize: Float = 12.0f,
      textScaleMode: TextScaleMode = TextScaleMode.Auto,
      textScaleMultiplier: Double = 1.0,
      enableLigatures: Boolean = true,
      textLigatures: Boolean = true,
      uiLigatures: Boolean = false
  ):
    def codeFontSize: Float =
      fontSize

    def codeLigatures: Boolean =
      enableLigatures

    def scaledCodeFontSize: Float =
      scaledPointSize(fontSize)

    def scaledTextFontSize: Float =
      scaledPointSize(textFontSize)

    def scaledUiFontSize: Float =
      scaledPointSize(uiFontSize)

    def resolveAutoTextScale(detectedDeviceScale: Double): FontConfig =
      textScaleMode match
        case TextScaleMode.Auto =>
          copy(textScaleMultiplier = FontConfig.clampTextScale(detectedDeviceScale.max(1.0)))
        case TextScaleMode.Manual =>
          copy(textScaleMultiplier = FontConfig.clampTextScale(textScaleMultiplier))
        case TextScaleMode.Off =>
          copy(textScaleMultiplier = 1.0)

    private def scaledPointSize(size: Float): Float =
      (size.toDouble * FontConfig.clampTextScale(textScaleMultiplier)).toFloat

  object FontConfig:
    val MinTextScale: Double = 0.5
    val MaxTextScale: Double = 4.0

    def clampTextScale(scale: Double): Double =
      scale.max(MinTextScale).min(MaxTextScale)

  lazy val availableMonospaceFamilies: List[String] =
    (BundledCodeFontFamily :: availableSystemFontFamilies.filter(f =>
      isMonospacedFamily(f) && canRenderBasicText(f)
    )).distinct

  lazy val availableTextFamilies: List[String] =
    (Font.SANS_SERIF :: availableSystemFontFamilies).filterNot(isMonospacedFamily).filter(canRenderBasicText).distinct

  lazy val availableUiFamilies: List[String] =
    availableTextFamilies

  /** The font families consulted when building the settings tree's font-family groups. `system` enumerates real fonts
    * installed on the running machine via
    * [[availableMonospaceFamilies]]/[[availableTextFamilies]]/[[availableUiFamilies]] -- tests that don't want their
    * assertions to depend on what happens to be installed on the machine running them (e.g. a CI runner shipping an
    * exotic font whose name collides with the search term under test) should build a [[FontFamilyCatalog]] of their own
    * instead and pass it to `CommandRunnerSettingsGroups.build` / `CommandRunner`.
    */
  final case class FontFamilyCatalog(monospace: List[String], text: List[String], ui: List[String])

  object FontFamilyCatalog:
    def system: FontFamilyCatalog =
      FontFamilyCatalog(availableMonospaceFamilies, availableTextFamilies, availableUiFamilies)

  /** Lists configured font roles whose requested families cannot be resolved by this runtime. */
  def missingFamilies(config: FontConfig): List[String] =
    List(
      ("code", config.codeFontFamily, availableMonospaceFamilies),
      ("text", config.textFontFamily, availableTextFamilies),
      ("UI", config.uiFontFamily, availableUiFamilies)
    ).collect {
      case (role, family, available) if !available.exists(_.equalsIgnoreCase(family)) =>
        s"$role font '$family'"
    }

  def isMonospacedFamily(family: String): Boolean =
    if family == BundledCodeFontFamily then true
    else isMonospaced(Font(family, Font.PLAIN, 12))

  def loadCodeFont(config: FontConfig)(using logger: Logger[IO]): IO[Font] =
    val size = config.scaledCodeFontSize
    val baseFontIO =
      if config.codeFontFamily == BundledCodeFontFamily then
        loadBundledMonospace(size).handleErrorWith { error =>
          logger.warn(s"Failed to load bundled code font, falling back to system fonts: ${error.getMessage}") *>
            IO.pure(defaultSystemMonospace(size))
        }
      else IO.pure(Font(config.codeFontFamily, Font.PLAIN, size.toInt).deriveFont(size))

    baseFontIO.map(applyFontFeatures(_, config.enableLigatures))

  def loadTextFont(config: FontConfig): IO[Font] =
    IO.pure(previewTextFont(config))

  def loadUiFont(config: FontConfig): IO[Font] =
    IO.pure(previewUiFont(config))

  def previewCodeFont(config: FontConfig): Font =
    val size = config.scaledCodeFontSize
    val base =
      if config.codeFontFamily == BundledCodeFontFamily then
        bundledMonospace(size).getOrElse(defaultSystemMonospace(size))
      else Font(config.codeFontFamily, Font.PLAIN, size.toInt).deriveFont(size)
    applyFontFeatures(base, config.enableLigatures)

  def previewTextFont(config: FontConfig): Font =
    val size = config.scaledTextFontSize
    applyFontFeatures(
      Font(config.textFontFamily, Font.PLAIN, size.toInt).deriveFont(size),
      config.textLigatures
    )

  def previewUiFont(config: FontConfig): Font =
    val size = config.scaledUiFontSize
    applyFontFeatures(
      Font(config.uiFontFamily, Font.PLAIN, size.toInt).deriveFont(size),
      config.uiLigatures
    )

  def previewFontForRole(config: FontConfig, role: TypographyRole): Font =
    role match
      case TypographyRole.Code            => previewCodeFont(config)
      case TypographyRole.Prose           => previewTextFont(config)
      case TypographyRole.MarkdownSource  => previewTextFont(config)
      case TypographyRole.MarkdownPreview => previewTextFont(config)
      case TypographyRole.Ui              => previewUiFont(config)
      case TypographyRole.Mixed           => previewTextFont(config)

  /** Loads the bundled toolbar icon font at the requested point size. */
  def toolbarIconFont(size: Float): Option[Font] =
    toolbarIconFontBase.map(_.deriveFont(size.max(1.0f)))

  /** Provides the registered toolbar icon font family when its bundled asset is available. */
  def toolbarIconFontFamily: Option[String] =
    toolbarIconFontBase.map(_.getFamily)

  def isMonospacedFont(font: Font): Boolean =
    isMonospaced(font)

  def ligaturesEnabled(font: Font): Boolean =
    Option(font.getAttributes.get(java.awt.font.TextAttribute.LIGATURES))
      .contains(java.awt.font.TextAttribute.LIGATURES_ON)

  /** Compatibility shim while the runtime moves to separate code/text fonts. */
  def loadMonaspaceNeon(config: FontConfig)(using logger: Logger[IO]): IO[List[Font]] =
    loadCodeFont(config).map(List(_))

  private lazy val availableSystemFontFamilies: List[String] =
    GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames.toList.sorted

  private def loadBundledMonospace(size: Float): IO[Font] =
    IO.pure(bundledMonospace(size).getOrElse(defaultSystemMonospace(size)))

  private def bundledMonospace(size: Float): Option[Font] =
    bundledFontBase.map(_.deriveFont(size))

  private lazy val bundledFontBase: Option[Font] =
    loadBundledFontBase("/fonts/MonaspaceNeonVarVF[wght,wdth,slnt].ttf")
      .orElse(loadBundledFontBase("/fonts/MonaspaceNeon-Regular.otf"))
      .orElse(loadBundledFontBase("/fonts/MonaspaceNeon-Bold.otf"))

  private lazy val toolbarIconFontBase: Option[Font] =
    loadBundledFontBase("/fonts/MaterialIconsRound-Regular.otf").filter { font =>
      try
        val environment = GraphicsEnvironment.getLocalGraphicsEnvironment
        environment.registerFont(font) || environment.getAvailableFontFamilyNames.contains(font.getFamily)
      catch case _: Exception => false
    }

  private def loadBundledFontBase(resourcePath: String): Option[Font] =
    Option(getClass.getResourceAsStream(resourcePath)).flatMap { stream =>
      try Some(Font.createFont(Font.TRUETYPE_FONT, stream))
      catch case _: Exception => None
      finally stream.close()
    }

  private def defaultSystemMonospace(size: Float): Font =
    availableMonospaceFamilies
      .find(_ != BundledCodeFontFamily)
      .map(family => Font(family, Font.PLAIN, size.toInt).deriveFont(size))
      .getOrElse(Font(Font.MONOSPACED, Font.PLAIN, size.toInt).deriveFont(size))

  private def canRenderBasicText(family: String): Boolean =
    try
      val font = Font(family, Font.PLAIN, 12)
      font.canDisplayUpTo("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") == -1
      && CellMetrics.fromFont(font.deriveFont(12.0f)).isValid
    catch case _: Exception => false

  private def isMonospaced(font: Font): Boolean =
    try
      val probeImage = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
      val graphics   = probeImage.createGraphics()
      try
        val metrics = graphics.getFontMetrics(font)
        val iWidth  = metrics.charWidth('i')
        val mWidth  = metrics.charWidth('m')
        val wWidth  = metrics.charWidth('W')
        iWidth == mWidth && mWidth == wWidth
      finally graphics.dispose()
    catch case _: Exception => false

  private def applyFontFeatures(font: Font, enableLigatures: Boolean): Font =
    if enableLigatures then font.deriveFont(createLigatureAttributes())
    else font

  private def createLigatureAttributes(): java.util.Map[java.awt.font.TextAttribute, Any] =
    val attributes = new java.util.HashMap[java.awt.font.TextAttribute, Any]()
    attributes.put(java.awt.font.TextAttribute.LIGATURES, java.awt.font.TextAttribute.LIGATURES_ON)
    attributes
