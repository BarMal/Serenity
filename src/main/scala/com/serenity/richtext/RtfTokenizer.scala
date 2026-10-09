package com.serenity.richtext

import java.nio.charset.StandardCharsets

import scala.annotation.tailrec

private[richtext] enum RtfToken:
  case GroupStart
  case GroupEnd
  case Word(name: String, parameter: Option[Int])
  case Symbol(char: Char)
  case HexByte(value: Int)
  case Text(value: String)

/** Splits RTF bytes into control words, symbols, hex escapes, group delimiters and literal text. Bytes of 0x80 and
  * above are surfaced as [[RtfToken.HexByte]] because RTF text is 7-bit and such bytes belong to the document code
  * page.
  */
private[richtext] object RtfTokenizer:
  private val Backslash = '\\'.toByte

  def tokenize(bytes: Array[Byte]): Either[RichTextCodecException, Vector[RtfToken]] =
    scan(bytes, 0, Vector.empty)

  @tailrec
  private def scan(
    bytes: Array[Byte],
    index: Int,
    tokens: Vector[RtfToken]
  ): Either[RichTextCodecException, Vector[RtfToken]] =
    if index >= bytes.length then Right(tokens)
    else
      val byte = bytes(index)
      if byte == '{' then scan(bytes, index + 1, tokens :+ RtfToken.GroupStart)
      else if byte == '}' then scan(bytes, index + 1, tokens :+ RtfToken.GroupEnd)
      else if byte == '\r' || byte == '\n' || byte == 0 then scan(bytes, index + 1, tokens)
      else if byte < 0 then scan(bytes, index + 1, tokens :+ RtfToken.HexByte(byte & 0xff))
      else if byte == Backslash then
        readControl(bytes, index) match
          case Left(error)              => Left(error)
          case Right((next, newTokens)) => scan(bytes, next, tokens ++ newTokens)
      else
        val end = literalEnd(bytes, index)
        scan(bytes, end, tokens :+ RtfToken.Text(String(bytes, index, end - index, StandardCharsets.ISO_8859_1)))

  private def literalEnd(bytes: Array[Byte], start: Int): Int =
    runEnd(bytes, start, byte => !isLiteralBoundary(byte))

  private def isLiteralBoundary(byte: Byte): Boolean =
    byte == '{' || byte == '}' || byte == Backslash || byte == '\r' || byte == '\n' || byte == 0 || byte < 0

  private def isLetter(byte: Byte): Boolean =
    (byte >= 'a' && byte <= 'z') || (byte >= 'A' && byte <= 'Z')

  private def isDigit(byte: Byte): Boolean =
    byte >= '0' && byte <= '9'

  private def truncated(what: String): Left[RichTextCodecException, Nothing] =
    Left(RichTextCodecException(s"RTF document is truncated inside $what"))

  private def readControl(
    bytes: Array[Byte],
    backslashIndex: Int
  ): Either[RichTextCodecException, (Int, Vector[RtfToken])] =
    val index = backslashIndex + 1
    if index >= bytes.length then truncated("a control sequence")
    else
      val byte = bytes(index)
      if isLetter(byte) then readWord(bytes, index)
      else if byte == '\'' then readHex(bytes, index + 1)
      else if byte == '\r' || byte == '\n' then Right((index + 1, Vector(RtfToken.Word("par", None))))
      else Right((index + 1, Vector(RtfToken.Symbol((byte & 0xff).toChar))))

  private def readHex(bytes: Array[Byte], digitsStart: Int): Either[RichTextCodecException, (Int, Vector[RtfToken])] =
    if digitsStart + 1 >= bytes.length then truncated("a hex escape")
    else
      val high = Character.digit(bytes(digitsStart).toInt, 16)
      val low  = Character.digit(bytes(digitsStart + 1).toInt, 16)
      if high < 0 || low < 0 then Left(RichTextCodecException("RTF document contains an invalid hex escape"))
      else Right((digitsStart + 2, Vector(RtfToken.HexByte(high * 16 + low))))

  private def readWord(bytes: Array[Byte], nameStart: Int): Either[RichTextCodecException, (Int, Vector[RtfToken])] =
    val nameEnd = runEnd(bytes, nameStart, isLetter)
    val name    = String(bytes, nameStart, nameEnd - nameStart, StandardCharsets.US_ASCII)
    val negative =
      nameEnd + 1 < bytes.length && bytes(nameEnd) == '-' && isDigit(bytes(nameEnd + 1))
    val digitsStart = if negative then nameEnd + 1 else nameEnd
    val digitsEnd   = runEnd(bytes, digitsStart, isDigit)
    val parameter   = Option.when(digitsEnd > digitsStart)(parseParameter(bytes, digitsStart, digitsEnd, negative))
    val next        = if digitsEnd < bytes.length && bytes(digitsEnd) == ' ' then digitsEnd + 1 else digitsEnd
    (name, parameter) match
      case ("bin", Some(length)) =>
        if next.toLong + length > bytes.length then truncated("binary data")
        else Right((next + length, Vector.empty))
      case _ => Right((next, Vector(RtfToken.Word(name, parameter))))

  private def runEnd(bytes: Array[Byte], start: Int, predicate: Byte => Boolean): Int =
    val offset = bytes.indexWhere(!predicate(_), start)
    if offset < 0 then bytes.length else offset

  private def parseParameter(bytes: Array[Byte], start: Int, end: Int, negative: Boolean): Int =
    val digits    = String(bytes, start, end - start, StandardCharsets.US_ASCII)
    val magnitude = digits.toLongOption.getOrElse(Int.MaxValue.toLong)
    val signed    = if negative then -magnitude else magnitude
    signed.max(Int.MinValue.toLong).min(Int.MaxValue.toLong).toInt
