package com.serenity.richtext

import java.nio.file.Path

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.io.AtomicFileWriter

/** Reads and writes RTF documents through Serenity's native rich text model, without Swing's `RTFEditorKit`. */
object RtfDocumentCodec:
  def read(path: Path): IO[RichTextDocument] =
    readWithFidelity(path).map(_.document)

  /** Read an RTF file and report structures that the native model cannot round-trip. */
  def readWithFidelity(path: Path): IO[RichTextImport] =
    RichTextArchive.readFile(path, "RTF").flatMap(bytes => IO.fromEither(readBytesWithFidelity(bytes)))

  def write(document: RichTextDocument, path: Path): IO[Unit] =
    AtomicFileWriter.writeBytes(path, writeBytes(document))

  def readBytes(bytes: Array[Byte]): Either[RichTextCodecException, RichTextDocument] =
    readBytesWithFidelity(bytes).map(_.document)

  /** Decode RTF bytes; malformed or truncated input is a `Left`, never an exception. */
  def readBytesWithFidelity(bytes: Array[Byte]): Either[RichTextCodecException, RichTextImport] =
    RichTextArchive.requireArchiveSize(bytes, "RTF").flatMap { _ =>
      Either
        .catchNonFatal(RtfParser.parse(bytes).map(RtfReader.read))
        .left
        .map(error => RichTextCodecException("RTF document could not be decoded", error))
        .flatten
    }

  /** Encode Serenity's native rich text model as RTF bytes. */
  def writeBytes(document: RichTextDocument): Array[Byte] =
    RtfWriter.write(document)
