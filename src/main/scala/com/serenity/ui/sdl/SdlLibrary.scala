package com.serenity.ui.sdl

import java.lang.foreign.{Arena, SymbolLookup}
import java.nio.file.Path

import scala.annotation.tailrec
import scala.util.Try

/** Where `libSDL3` comes from: the copy the package bundles beside the jar, else whatever the system loader finds. */
object SdlLibrary:

  /** A directory holding [[FileName]], or the library file itself. The packages set it to `$APPDIR/natives`. */
  val PathProperty: String = "serenity.sdl3.library.path"

  val FileName: String = "libSDL3.so.0"

  enum Source:
    case Bundled(file: Path)
    case System(name: String)

  /** The sources to try, best first. The system library is always the last resort. */
  def candidates(properties: Map[String, String], fileExists: Path => Boolean): List[Source] =
    val bundled = properties
      .get(PathProperty)
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(Path.of(_))
      .flatMap(configured =>
        List(configured.resolve(FileName), configured)
          .find(file => fileExists(file) && file.getFileName.toString.contains(".so"))
      )
    bundled.map(Source.Bundled(_)).toList :+ Source.System(FileName)

  /** The first source `open` accepts, with what it opened; every refusal otherwise. */
  def load[A](candidates: List[Source], open: Source => Either[String, A]): Either[SdlError, (Source, A)] =
    @tailrec def go(rest: List[Source], failures: List[String]): Either[SdlError, (Source, A)] =
      rest match
        case Nil => Left(SdlError.LibraryUnavailable(failures.reverse))
        case source :: tail =>
          open(source) match
            case Right(opened) => Right((source, opened))
            case Left(failure) => go(tail, failure :: failures)
    go(candidates, Nil)

  /** Opens `source` for symbol lookup; the library stays loaded until `arena` closes. */
  def open(source: Source, arena: Arena): Either[String, SymbolLookup] =
    val attempt = source match
      case Source.Bundled(file) => Try(SymbolLookup.libraryLookup(file, arena))
      case Source.System(name)  => Try(SymbolLookup.libraryLookup(name, arena))
    attempt.toEither.left.map(failure => Option(failure.getMessage).getOrElse(failure.getClass.getName))
