package com.serenity.exporting

import java.io.IOException

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.manuscript.typography.{FaceStyle, FontFamily}
import org.apache.fontbox.ttf.{TTFParser, TrueTypeFont}
import org.apache.pdfbox.io.RandomAccessReadBuffer

final case class FontLoadError(resource: String, reason: String) extends Exception(s"$resource: $reason")

/** The font files bundled in the jar. The measurer reads the same bytes the PDF painter will embed. */
object BundledFonts:

  def read(family: FontFamily, style: FaceStyle): IO[Array[Byte]] =
    readResource(family.resource(style))

  def readResource(resource: String): IO[Array[Byte]] =
    IO.blocking(readBytes(resource)).flatMap(IO.fromEither)

  def parse(resource: String, bytes: Array[Byte]): Either[FontLoadError, TrueTypeFont] =
    Either
      .catchOnly[IOException](TTFParser().parse(RandomAccessReadBuffer(bytes)))
      .leftMap(failure => FontLoadError(resource, Option(failure.getMessage).getOrElse("unreadable font")))

  private def readBytes(resource: String): Either[FontLoadError, Array[Byte]] =
    Option(getClass.getResourceAsStream(resource))
      .toRight(FontLoadError(resource, "not on the classpath"))
      .flatMap { stream =>
        Either
          .catchOnly[IOException](stream.use(_.readAllBytes()))
          .leftMap(failure => FontLoadError(resource, Option(failure.getMessage).getOrElse("unreadable")))
      }

  extension (stream: java.io.InputStream)

    private def use[A](read: java.io.InputStream => A): A =
      try read(stream)
      finally stream.close()
