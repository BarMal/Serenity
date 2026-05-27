package com.serenity.ui.fonts

import java.awt.Font

import cats.effect.IO
import org.typelevel.log4cats.Logger

object FontLoader:

  case class FontConfig(
      useCustomFont: Boolean = true,
      fontSize: Float = 12.0f,
      enableLigatures: Boolean = true
  )

  /** Load Monaspace Neon font from resources with fallback to system monospace fonts */
  def loadMonaspaceNeon(config: FontConfig)(using logger: Logger[IO]): IO[List[Font]] =
    if config.useCustomFont then
      loadCustomFonts(config).handleErrorWith { error =>
        logger.warn(s"Failed to load custom fonts, falling back to system fonts: ${error.getMessage}") *>
          IO.pure(getSystemMonospaceFonts(config.fontSize))
      }
    else IO.pure(getSystemMonospaceFonts(config.fontSize))

  /** Load custom fonts from resources with proper error handling */
  private def loadCustomFonts(config: FontConfig)(using Logger[IO]): IO[List[Font]] =
    for
      _           <- IO.unit
      regularFont <- loadFontFromResource("/fonts/MonaspaceNeon-Regular.otf", config.fontSize)
      boldFont    <- loadFontFromResource("/fonts/MonaspaceNeon-Bold.otf", config.fontSize)
      varFont     <- loadVariableFont(config.fontSize)
    yield List(varFont, regularFont, boldFont).flatten

  /** Load a font from classpath resources */
  private def loadFontFromResource(resourcePath: String, size: Float): IO[Option[Font]] =
    IO.blocking {
      Option(getClass.getResourceAsStream(resourcePath)).flatMap { stream =>
        try
          val font = Font.createFont(Font.TRUETYPE_FONT, stream).deriveFont(size)
          Some(font)
        catch case _: Exception => None
        finally stream.close()
      }
    }

  /** Load variable font with weight and width support */
  private def loadVariableFont(size: Float): IO[Option[Font]] =
    IO.blocking {
      Option(getClass.getResourceAsStream("/fonts/MonaspaceNeonVarVF[wght,wdth,slnt].ttf")).flatMap { stream =>
        try
          val font = Font.createFont(Font.TRUETYPE_FONT, stream).deriveFont(size)
          Some(font)
        catch case _: Exception => None
        finally stream.close()
      }
    }

  /** Get system monospace fonts as fallback */
  private def getSystemMonospaceFonts(size: Float): List[Font] =
    val systemFonts = List(
      Font(Font.MONOSPACED, Font.PLAIN, size.toInt),
      Font("Monaco", Font.PLAIN, size.toInt),
      Font("Menlo", Font.PLAIN, size.toInt),
      Font("Consolas", Font.PLAIN, size.toInt),
      Font("DejaVu Sans Mono", Font.PLAIN, size.toInt),
      Font("Liberation Mono", Font.PLAIN, size.toInt)
    )
    systemFonts.filter(isMonospaced)

  /** Check if a font is monospaced by comparing character widths */
  private def isMonospaced(font: Font): Boolean =
    try
      val metrics = java.awt.Toolkit.getDefaultToolkit.getFontMetrics(font)
      val iWidth  = metrics.charWidth('i')
      val mWidth  = metrics.charWidth('m')
      val wWidth  = metrics.charWidth('W')
      iWidth == mWidth && mWidth == wWidth
    catch case _: Exception => false

  /** Create font attributes for ligature support (for future use) */
  private def createLigatureAttributes(): java.util.Map[java.awt.font.TextAttribute, Any] =
    val attributes = new java.util.HashMap[java.awt.font.TextAttribute, Any]()
    attributes.put(java.awt.font.TextAttribute.LIGATURES, java.awt.font.TextAttribute.LIGATURES_ON)
    attributes.put(java.awt.font.TextAttribute.KERNING, java.awt.font.TextAttribute.KERNING_ON)
    attributes
