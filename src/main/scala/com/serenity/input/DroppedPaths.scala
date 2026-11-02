package com.serenity.input

import java.awt.datatransfer.{DataFlavor, Transferable}
import java.io.{BufferedReader, File, InputStream, Reader}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import cats.syntax.either.*

/** What a drag onto the window asks the editor to open. A file manager offers its files as a Java file list, or on some
  * toolkits and bridges only as a `text/uri-list`, which can reach the drop as a string, a stream or a reader.
  */
object DroppedPaths:

  private val UriListMimeType = "text/uri-list"

  /** The paths the drag carries: its file list, or failing that its URI list. A drag whose data cannot be read has none
    * -- the AWT calls here throw on a drag the source has already abandoned, which must not reach the caller.
    */
  def from(transferable: Transferable): List[Path] =
    fileList(transferable) match
      case Nil   => uriList(transferable)
      case paths => paths

  def offersPaths(flavors: Array[DataFlavor]): Boolean =
    flavors.exists(flavor => flavor.equals(DataFlavor.javaFileListFlavor) || isUriList(flavor))

  private def isUriList(flavor: DataFlavor): Boolean = flavor.isMimeTypeEqual(UriListMimeType)

  private def dataFor(transferable: Transferable, flavor: DataFlavor): Option[AnyRef] =
    if transferable.isDataFlavorSupported(flavor) then
      Either.catchNonFatal(transferable.getTransferData(flavor)).toOption.flatMap(Option(_))
    else None

  private def fileList(transferable: Transferable): List[Path] =
    dataFor(transferable, DataFlavor.javaFileListFlavor).toList.flatMap {
      case files: java.util.List[?] => files.asScala.toList.collect { case file: File => file.toPath }
      case _                        => Nil
    }

  private def uriList(transferable: Transferable): List[Path] =
    transferable.getTransferDataFlavors
      .find(isUriList)
      .flatMap(dataFor(transferable, _))
      .flatMap(textOf)
      .toList
      .flatMap(paths)

  private def textOf(data: AnyRef): Option[String] =
    data match
      case text: String => Some(text)
      case stream: InputStream =>
        Either
          .catchNonFatal(Using.resource(stream)(open => new String(open.readAllBytes(), StandardCharsets.UTF_8)))
          .toOption
      case reader: Reader =>
        Either
          .catchNonFatal(Using.resource(new BufferedReader(reader))(_.lines().iterator().asScala.mkString("\n")))
          .toOption
      case _ => None

  /** RFC 2483: one URI a line, `#` starts a comment. Only `file:` URIs name something the editor can open. */
  private def paths(uriList: String): List[Path] =
    uriList.linesIterator.map(_.trim).filterNot(line => line.isEmpty || line.startsWith("#")).flatMap(pathOf).toList

  private def pathOf(line: String): Option[Path] =
    Either
      .catchNonFatal(new URI(line))
      .toOption
      .filter(uri => "file".equalsIgnoreCase(uri.getScheme))
      .flatMap(uri => Either.catchNonFatal(Paths.get(uri)).toOption)
