package com.serenity.testkit

import com.serenity.command.CommandRunner
import com.serenity.ui.fonts.FontLoader.FontFamilyCatalog

/** A font catalog that does not depend on the machine's fonts, for specs that build a command runner but do not search
  * the font families.
  */
object FixedFontCatalog:

  val catalog: FontFamilyCatalog = FontFamilyCatalog(monospace = List("Mono"), text = List("Serif"), ui = List("Sans"))

  /** `CommandRunner.empty` with this catalog; `empty` itself reads the installed fonts, and `copy` would be too late.
    */
  def emptyRunner: CommandRunner = CommandRunner(isActive = false, fontFamilies = catalog)
