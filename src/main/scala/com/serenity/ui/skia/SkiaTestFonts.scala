package com.serenity.ui.skia

import scala.util.Using

import com.serenity.ui.renderer.BackendCaches
import org.jetbrains.skia.{Data, FontMgr, Typeface}

/** For specs only: gives one cache owner a font from the classpath that the system font manager does not hold. */
object SkiaTestFonts:

  /** Installs the font at `resource` for `owner`, before its Skia caches are first used, and returns its family, or why
    * the font could not be read.
    */
  private[serenity] def install(owner: BackendCaches, resource: String): Either[String, String] =
    load(resource).map { typeface =>
      val family = typeface.getFamilyName
      SkiaTextCaches.withTypefaces(
        owner,
        key => if key.family.equalsIgnoreCase(family) then typeface else SkiaTypefaces.typefaceFor(key)
      )
      family
    }

  private def load(resource: String): Either[String, Typeface] =
    for
      stream <- Option(getClass.getResourceAsStream(resource)).toRight(s"no font resource $resource")
      bytes <- Using(stream)(_.readAllBytes()).toEither.left.map(error => s"cannot read $resource: ${error.getMessage}")
      face <- Option(
        FontMgr.Companion.getDefault().makeFromData(Data.Companion.makeFromBytes(bytes, 0, bytes.length), 0)
      )
        .toRight(s"Skia cannot read the font $resource")
    yield face
