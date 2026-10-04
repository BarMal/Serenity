package com.serenity.text

import java.nio.charset.{Charset, StandardCharsets}

/** The character encoding a text file arrived in (#1627), recorded so saving writes it back in the same bytes rather
  * than re-encoding everything as UTF-8.
  */
enum TextEncoding(val charset: Charset, val byteOrderMark: Vector[Byte]):
  case Utf8        extends TextEncoding(StandardCharsets.UTF_8, Vector(0xef.toByte, 0xbb.toByte, 0xbf.toByte))
  case Utf16Be     extends TextEncoding(StandardCharsets.UTF_16BE, Vector(0xfe.toByte, 0xff.toByte))
  case Utf16Le     extends TextEncoding(StandardCharsets.UTF_16LE, Vector(0xff.toByte, 0xfe.toByte))
  case Windows1252 extends TextEncoding(Charset.forName("windows-1252"), Vector.empty[Byte])
  case Iso88591    extends TextEncoding(StandardCharsets.ISO_8859_1, Vector.empty[Byte])

  def configKey: String = charset.name()

object TextEncoding:

  val default: TextEncoding = Utf8

  def fromConfigKey(value: String): Option[TextEncoding] =
    values.find(_.configKey.equalsIgnoreCase(value.trim))
