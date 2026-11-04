package com.serenity.ui.fonts

/** The prose, code and UI font sizes in points, which change together or one at a time and are remembered together for
  * undo (#2405).
  */
final case class FontSizes(prose: Float, code: Float, ui: Float):
  def clamped: FontSizes = FontSizes(FontSizes.clamp(prose), FontSizes.clamp(code), FontSizes.clamp(ui))

object FontSizes:
  val Min: Float = 8.0f
  val Max: Float = 48.0f

  def clamp(size: Float): Float = size.max(Min).min(Max)

  def all(size: Float): FontSizes = FontSizes(size, size, size).clamped

  /** `prose, code, ui` as the config file writes them; none for anything that is not exactly three numbers. */
  def parse(text: String): Option[FontSizes] =
    text.split(',').toList.map(_.trim.toFloatOption) match
      case List(Some(prose), Some(code), Some(ui)) => Some(FontSizes(prose, code, ui).clamped)
      case _                                       => None

  def render(sizes: FontSizes): String = s"${sizes.prose}, ${sizes.code}, ${sizes.ui}"
