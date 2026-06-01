package com.serenity.ui.fonts

import java.awt.{Font, GraphicsEnvironment, Toolkit}

import cats.effect.IO
import org.typelevel.log4cats.Logger

object FontLoader:

  val BundledCodeFontFamily = "Monaspace Neon (Bundled)"

  case class FontConfig(
      codeFontFamily: String = BundledCodeFontFamily,
      textFontFamily: String = Font.SANS_SERIF,
      fontSize: Float = 12.0f,
      enableLigatures: Boolean = true
  )

  lazy val availableMonospaceFamilies: List[String] =
    (BundledCodeFontFamily :: availableSystemFontFamilies.filter(isMonospacedFamily)).distinct

  lazy val availableTextFamilies: List[String] =
    ((Font.SANS_SERIF :: availableSystemFontFamilies).filterNot(isMonospacedFamily)).distinct

  def isMonospacedFamily(family: String): Boolean =
    if family == BundledCodeFontFamily then true
    else isMonospaced(Font(family, Font.PLAIN, 12))

  def loadCodeFont(config: FontConfig)(using logger: Logger[IO]): IO[Font] =
    val size = config.fontSize
    val baseFontIO =
      if config.codeFontFamily == BundledCodeFontFamily then
        loadBundledMonospace(size).handleErrorWith { error =>
          logger.warn(s"Failed to load bundled code font, falling back to system fonts: ${error.getMessage}") *>
            IO.pure(defaultSystemMonospace(size))
        }
      else IO.pure(Font(config.codeFontFamily, Font.PLAIN, size.toInt).deriveFont(size))

    baseFontIO.map(applyFontFeatures(_, config.enableLigatures))

  def loadTextFont(config: FontConfig)(using Logger[IO]): IO[Font] =
    IO.pure(previewTextFont(config))

  def previewCodeFont(config: FontConfig): Font =
    val base =
      if config.codeFontFamily == BundledCodeFontFamily then defaultSystemMonospace(config.fontSize)
      else Font(config.codeFontFamily, Font.PLAIN, config.fontSize.toInt).deriveFont(config.fontSize)
    applyFontFeatures(base, config.enableLigatures)

  def previewTextFont(config: FontConfig): Font =
    applyFontFeatures(
      Font(config.textFontFamily, Font.PLAIN, config.fontSize.toInt).deriveFont(config.fontSize),
      config.enableLigatures
    )

  /** Compatibility shim while the runtime moves to separate code/text fonts. */
  def loadMonaspaceNeon(config: FontConfig)(using logger: Logger[IO]): IO[List[Font]] =
    loadCodeFont(config).map(List(_))

  private lazy val availableSystemFontFamilies: List[String] =
    GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames.toList.sorted

  private def loadBundledMonospace(size: Float)(using Logger[IO]): IO[Font] =
    loadBundledFonts(size).map(_.headOption.getOrElse(defaultSystemMonospace(size)))

  private def loadBundledFonts(size: Float)(using Logger[IO]): IO[List[Font]] =
    for
      regularFont <- loadFontFromResource("/fonts/MonaspaceNeon-Regular.otf", size)
      boldFont    <- loadFontFromResource("/fonts/MonaspaceNeon-Bold.otf", size)
      varFont     <- loadVariableFont(size)
    yield List(varFont, regularFont, boldFont).flatten

  private def loadFontFromResource(resourcePath: String, size: Float): IO[Option[Font]] =
    IO.blocking {
      Option(getClass.getResourceAsStream(resourcePath)).flatMap { stream =>
        try
          Some(Font.createFont(Font.TRUETYPE_FONT, stream).deriveFont(size))
        catch case _: Exception => None
        finally stream.close()
      }
    }

  private def loadVariableFont(size: Float): IO[Option[Font]] =
    IO.blocking {
      Option(getClass.getResourceAsStream("/fonts/MonaspaceNeonVarVF[wght,wdth,slnt].ttf")).flatMap { stream =>
        try
          Some(Font.createFont(Font.TRUETYPE_FONT, stream).deriveFont(size))
        catch case _: Exception => None
        finally stream.close()
      }
    }

  private def defaultSystemMonospace(size: Float): Font =
    availableMonospaceFamilies
      .find(_ != BundledCodeFontFamily)
      .map(family => Font(family, Font.PLAIN, size.toInt).deriveFont(size))
      .getOrElse(Font(Font.MONOSPACED, Font.PLAIN, size.toInt).deriveFont(size))

  private def isMonospaced(font: Font): Boolean =
    try
      val metrics = Toolkit.getDefaultToolkit.getFontMetrics(font)
      val iWidth  = metrics.charWidth('i')
      val mWidth  = metrics.charWidth('m')
      val wWidth  = metrics.charWidth('W')
      iWidth == mWidth && mWidth == wWidth
    catch case _: Exception => false

  private def applyFontFeatures(font: Font, enableLigatures: Boolean): Font =
    if enableLigatures then font.deriveFont(createLigatureAttributes())
    else font

  private def createLigatureAttributes(): java.util.Map[java.awt.font.TextAttribute, Any] =
    val attributes = new java.util.HashMap[java.awt.font.TextAttribute, Any]()
    attributes.put(java.awt.font.TextAttribute.LIGATURES, java.awt.font.TextAttribute.LIGATURES_ON)
    attributes.put(java.awt.font.TextAttribute.KERNING, java.awt.font.TextAttribute.KERNING_ON)
    attributes
