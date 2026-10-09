package com.serenity.io

import java.nio.file.{Files, Path}

import scala.util.control.NonFatal

/** What the macOS Open... asks of AppKit, in order: an `NSOpenPanel` that takes one file or folder, started in a
  * directory, run modally, and its URL read back as a path. Everything sits inside an autorelease pool because the
  * calling thread is the AppKit thread, whose own pool is not ours to drain.
  *
  * `canChooseFiles` and `canChooseDirectories` are both on. The AWT dialog cannot do that: the JDK's `CFileDialog.m`
  * sets them from one flag, `setCanChooseFiles:!fChooseDirectories` and `setCanChooseDirectories:fChooseDirectories`.
  */
private[io] object OpenPanelScript:

  private val NSModalResponseOK = 1L

  def chose(response: Long): Boolean = response == NSModalResponseOK

  def run(objc: ObjectiveC, initialDirectory: Option[Path]): Either[String, Option[Path]] =
    classes(objc).flatMap(withinPool(objc, _, initialDirectory))

  final private case class Classes(pool: Long, panel: Long, string: Long, url: Long)

  private def classes(objc: ObjectiveC): Either[String, Classes] =
    def named(name: String): Either[String, Long] =
      Some(objc.classNamed(name)).filter(_ != 0L).toRight(s"The Objective-C class $name is not available")
    for
      pool   <- named("NSAutoreleasePool")
      panel  <- named("NSOpenPanel")
      string <- named("NSString")
      url    <- named("NSURL")
    yield Classes(pool, panel, string, url)

  private def withinPool(
    objc: ObjectiveC,
    classes: Classes,
    initialDirectory: Option[Path]
  ): Either[String, Option[Path]] =
    attempt(objc.send(objc.send(classes.pool, "alloc"), "init")).flatMap {
      case 0L => Left("Could not create an autorelease pool")
      case pool =>
        try attempt(choose(objc, classes, initialDirectory)).flatten
        finally tell(objc, pool, "drain")
    }

  private def choose(objc: ObjectiveC, classes: Classes, initialDirectory: Option[Path]): Either[String, Option[Path]] =
    objc.send(classes.panel, "openPanel") match
      case 0L => Left("NSOpenPanel could not be created")
      case panel =>
        tell(objc, panel, "setCanChooseFiles:", 1L)
        tell(objc, panel, "setCanChooseDirectories:", 1L)
        tell(objc, panel, "setAllowsMultipleSelection:", 0L)
        startIn(objc, classes, panel, initialDirectory)
        if chose(objc.send(panel, "runModal")) then selection(objc, panel) else Right(None)

  private def startIn(objc: ObjectiveC, classes: Classes, panel: Long, initialDirectory: Option[Path]): Unit =
    initialDirectory.map(_.toAbsolutePath.normalize()).filter(Files.isDirectory(_)).foreach { directory =>
      objc.withUtf8(directory.toString) { bytes =>
        val text = objc.send(classes.string, "stringWithUTF8String:", bytes)
        val url  = objc.send(classes.url, "fileURLWithPath:isDirectory:", text, 1L)
        tell(objc, panel, "setDirectoryURL:", url)
      }
    }

  // The UTF-8 bytes belong to an autoreleased NSString, so they are copied into a Java String before the pool drains.
  private def selection(objc: ObjectiveC, panel: Long): Either[String, Option[Path]] =
    for
      url   <- nonNil(objc.send(panel, "URL"), "NSOpenPanel answered OK without a URL")
      path  <- nonNil(objc.send(url, "path"), "The URL NSOpenPanel chose has no path")
      bytes <- nonNil(objc.send(path, "UTF8String"), "The path NSOpenPanel chose has no UTF-8 form")
      text  <- objc.readUtf8(bytes).filter(_.nonEmpty).toRight("The path NSOpenPanel chose is empty")
    yield Some(Path.of(text).normalize())

  private def nonNil(value: Long, otherwise: String): Either[String, Long] =
    Some(value).filter(_ != 0L).toRight(otherwise)

  private def tell(objc: ObjectiveC, receiver: Long, selector: String, arguments: Long*): Unit =
    val _ = objc.send(receiver, selector, arguments*)

  private def attempt[A](body: => A): Either[String, A] =
    try Right(body)
    catch
      case NonFatal(failure)     => Left(describe(failure))
      case failure: LinkageError => Left(describe(failure))

  private def describe(failure: Throwable): String =
    Option(failure.getMessage).getOrElse(failure.getClass.getSimpleName)
