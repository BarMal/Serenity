package com.serenity.ui.layout

import java.awt.font.TextAttribute
import java.awt.{Font, GraphicsEnvironment}

/** A pinned Inter subset with its GSUB/GPOS tables, so tests of shaping do not depend on which font the platform maps
  * the logical families to.
  */
object BundledInterFont:

  private val Resource = "/fonts/Inter-Regular-Subset.ttf"

  def plain(sizePx: Float): Font =
    val stream = getClass.getResourceAsStream(Resource)
    try Font.createFont(Font.TRUETYPE_FONT, stream).deriveFont(sizePx)
    finally stream.close()

  /** Makes the subset answer to its family name in AWT on every machine, so a font configuration naming `Inter` gets
    * it, not whichever face the machine maps an unknown name to.
    */
  lazy val registered: Boolean =
    val stream = getClass.getResourceAsStream(Resource)
    try GraphicsEnvironment.getLocalGraphicsEnvironment.registerFont(Font.createFont(Font.TRUETYPE_FONT, stream))
    finally stream.close()

  def ligatured(sizePx: Float): Font =
    plain(sizePx).deriveFont(java.util.Map.of(TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON))
