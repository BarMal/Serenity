package com.serenity.animation.sprite

/** Which bundled sprite sheet the companion pane draws. A closed, named set rather than a free-form resource path so a
  * config file can only ever name a character this build actually ships -- adding a new one (the licensed sprite sheet
  * this feature was inspired by among them, once one exists to bundle) is one more `case` here, not an API change to
  * [[CompanionSpriteConfig]] or anything that reads it.
  */
enum CompanionCharacter(val id: String, val sheetResourcePath: String):
  case PixelWizard extends CompanionCharacter("pixel-wizard", "/sprites/pixel-wizard-idle.png")

object CompanionCharacter:

  val default: CompanionCharacter = PixelWizard

  def fromConfigKey(value: String): Option[CompanionCharacter] =
    val normalized = value.trim.toLowerCase
    values.find(_.id == normalized)
