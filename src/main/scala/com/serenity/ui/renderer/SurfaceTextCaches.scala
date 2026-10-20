package com.serenity.ui.renderer

/** The caches a Java2D surface paints text through. Their owner shares one set across every frame and layer it draws,
  * so what one frame shaped, derived or rasterised the next finds; a surface built without an owner gets its own.
  */
final case class SurfaceTextCaches(
    labelSprites: LabelSprites,
    styledFonts: StyledFontCache,
    shapedText: ShapedTextCache
)

object SurfaceTextCaches:

  def fresh(): SurfaceTextCaches =
    SurfaceTextCaches(LabelSprites(LabelSprites.DefaultCapacity), StyledFontCache(), ShapedTextCache())
