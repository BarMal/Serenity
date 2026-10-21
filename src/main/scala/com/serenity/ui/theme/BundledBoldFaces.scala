package com.serenity.ui.theme

import java.awt.Font

/** The Bold face of each bundled code font, loaded from the file the font ships with. A font made by `Font.createFont`
  * keeps the one file it was made from, so asking it for bold gets the JDK's synthetic bold, whose advances are about
  * 5% wider than the regular's and so carry text off a cell grid. The bundled Bold files have the regular's advances
  * exactly (checked in `TextStyleBoldFaceSpec`), so bold drawn with them stays on the grid.
  *
  * Lives in the theme package, beside [[TextStyle.styledFont]] which asks it, because the fonts package already depends
  * on this one.
  */
object BundledBoldFaces:

  final private case class Bundled(families: Set[String], boldResource: String)

  // `Monaspace Neon Var` is the variable file the code font is loaded from; the static Regular is `Monaspace Neon`.
  private val bundled = List(
    Bundled(Set("Monaspace Neon Var", "Monaspace Neon"), "/fonts/MonaspaceNeon-Bold.otf"),
    Bundled(Set("Courier Prime"), "/fonts/CourierPrime-Bold.ttf")
  )

  private lazy val facesByFamily: Map[String, Font] =
    bundled
      .flatMap(entry => load(entry.boldResource).toList.flatMap(face => entry.families.toList.map(_ -> face)))
      .toMap

  /** The Bold face to draw `base` in when it is bold, if `base` is a bundled font. */
  def of(base: Font): Option[Font] = facesByFamily.get(base.getFamily)

  private def load(resource: String): Option[Font] =
    Option(getClass.getResourceAsStream(resource)).flatMap { stream =>
      try Some(Font.createFont(Font.TRUETYPE_FONT, stream))
      catch case _: Exception => None
      finally stream.close()
    }
