package com.serenity.publish

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.io.{AtomicFileWriter, FileType}
import com.serenity.manuscript.docx.ManuscriptDocxWriter
import com.serenity.manuscript.{CompileError, CompileSpec, ManuscriptCompiler, ManuscriptConf, SourceDocument}
import com.serenity.richtext.{DocxDocumentCodec, OdtDocumentCodec, RtfDocumentCodec}

final class ManuscriptExportException(val error: CompileError) extends RuntimeException(error.message)

/** The document an export starts from: a snapshot of the focused buffer, and the file it was read from, if any. */
final case class ExportOrigin(path: Option[Path], snapshot: SourceDocument)

/** The shell around the pure compiler and writer: finds `manuscript.conf`, reads the sources it names, and writes the
  * DOCX. Nothing here touches the editor's buffers; the origin is a snapshot taken before the export starts.
  */
object ManuscriptExport:

  /** The draft's own name is never offered, so an export cannot overwrite the source it came from. */
  def suggestedFileName(origin: Option[Path]): String =
    origin.flatMap(stemOf).fold("manuscript.docx")(stem => s"$stem-manuscript.docx")

  def writeDocx(origin: ExportOrigin, target: Path): IO[Unit] =
    compiledDocx(origin).flatMap(AtomicFileWriter.writeBytes(target, _))

  /** With a `manuscript.conf` beside the origin that lists sources, those are the book, read in order -- the origin's
    * own entry from the snapshot, so unsaved edits are exported. Otherwise the book is the origin alone.
    */
  def compiledDocx(origin: ExportOrigin): IO[Array[Byte]] =
    for
      spec <- specFor(origin)
      sources <- spec.includedSourcePaths match
        case Nil   => IO.pure(List(origin.snapshot))
        case paths => paths.traverse(readSource(origin, confDirectory(origin), _))
      manuscript <- IO.fromEither(ManuscriptCompiler.compile(spec, sources).leftMap(ManuscriptExportException(_)))
    yield ManuscriptDocxWriter.write(manuscript, spec.format)

  private def confDirectory(origin: ExportOrigin): Option[Path] =
    origin.path.flatMap(path => Option(path.toAbsolutePath.getParent))

  private def specFor(origin: ExportOrigin): IO[CompileSpec] =
    val defaults = CompileSpec.forTitle(origin.path.flatMap(stemOf).getOrElse("Untitled"))
    confDirectory(origin).map(_.resolve(ManuscriptConf.FileName)) match
      case None => IO.pure(defaults)
      case Some(confPath) =>
        IO.blocking(Files.isRegularFile(confPath)).flatMap {
          case false => IO.pure(defaults)
          case true =>
            IO.blocking(Files.readString(confPath, StandardCharsets.UTF_8))
              .flatMap(text =>
                IO.fromEither(ManuscriptConf.decode(text, defaults).leftMap(ManuscriptExportException(_)))
              )
        }

  private def readSource(origin: ExportOrigin, directory: Option[Path], relative: String): IO[SourceDocument] =
    val path = directory.fold(Path.of(relative))(_.resolve(relative)).normalize
    if origin.path.exists(_.toAbsolutePath.normalize == path.toAbsolutePath) then IO.pure(origin.snapshot)
    else
      FileType.fromPath(path) match
        case FileType.WordOpenXmlDocument => DocxDocumentCodec.read(path).map(SourceDocument.Rich(_))
        case FileType.OpenDocumentText    => OdtDocumentCodec.read(path).map(SourceDocument.Rich(_))
        case FileType.RichText            => RtfDocumentCodec.read(path).map(SourceDocument.Rich(_))
        case _ => IO.blocking(Files.readString(path, StandardCharsets.UTF_8)).map(SourceDocument.Markdown(_))

  private def stemOf(path: Path): Option[String] =
    Option(path.getFileName).map(_.toString).map { name =>
      val dot = name.lastIndexOf('.')
      if dot > 0 then name.substring(0, dot) else name
    }
