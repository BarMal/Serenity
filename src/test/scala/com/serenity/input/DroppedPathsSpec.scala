package com.serenity.input

import java.awt.datatransfer.DataFlavor
import java.io.{ByteArrayInputStream, IOException, StringReader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}

import scala.jdk.CollectionConverters.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2388: what a drag carries, as the paths it asks the editor to open. */
class DroppedPathsSpec extends AnyFlatSpec with Matchers:

  private val uriListAsString = new DataFlavor("text/uri-list;class=java.lang.String")
  private val uriListAsStream = new DataFlavor("text/uri-list;class=java.io.InputStream")
  private val uriListAsReader = new DataFlavor("text/uri-list;class=java.io.Reader")

  private val directory = Paths.get(System.getProperty("java.io.tmpdir"))
  private val notes     = directory.resolve("notes.md")
  private val spaced    = directory.resolve("a b.txt")

  private def fileList(paths: Path*): (DataFlavor, () => AnyRef) =
    DataFlavor.javaFileListFlavor -> (() => paths.map(_.toFile).asJava)

  private def uriText(flavor: DataFlavor, text: String): (DataFlavor, () => AnyRef) =
    val data: () => AnyRef = flavor match
      case `uriListAsStream` => () => new ByteArrayInputStream(text.getBytes(StandardCharsets.UTF_8))
      case `uriListAsReader` => () => new StringReader(text)
      case _                 => () => text
    flavor -> data

  "DroppedPaths.from" should "read the paths from a file-list drag" in {
    val drag = new FakeTransferable(List(fileList(notes, spaced)))

    DroppedPaths.from(drag) shouldBe List(notes, spaced)
  }

  it should "prefer the file list when a drag offers both it and a URI list" in {
    val drag = new FakeTransferable(List(fileList(notes), uriText(uriListAsString, spaced.toUri.toString)))

    DroppedPaths.from(drag) shouldBe List(notes)
  }

  it should "read file URIs from a URI-list string, decoding what the URI escapes" in {
    val drag = new FakeTransferable(List(uriText(uriListAsString, s"${notes.toUri}\r\n${spaced.toUri}\r\n")))

    DroppedPaths.from(drag) shouldBe List(notes, spaced)
  }

  it should "read a URI list offered as a stream or a reader" in {
    val text = s"${notes.toUri}\n"

    DroppedPaths.from(new FakeTransferable(List(uriText(uriListAsStream, text)))) shouldBe List(notes)
    DroppedPaths.from(new FakeTransferable(List(uriText(uriListAsReader, text)))) shouldBe List(notes)
  }

  it should "skip comment lines, blank lines and URIs that are not files" in {
    val text = s"# dragged from a file manager\n\nhttps://example.com/notes.md\n${notes.toUri}\n"
    val drag = new FakeTransferable(List(uriText(uriListAsString, text)))

    DroppedPaths.from(drag) shouldBe List(notes)
  }

  it should "skip a line that is not a URI at all" in {
    val drag = new FakeTransferable(List(uriText(uriListAsString, s"not a uri\n${notes.toUri}\n")))

    DroppedPaths.from(drag) shouldBe List(notes)
  }

  it should "give nothing for a drag that offers neither a file list nor a URI list" in {
    val drag = new FakeTransferable(List(DataFlavor.stringFlavor -> (() => "just some text")))

    DroppedPaths.from(drag) shouldBe Nil
  }

  it should "give nothing for an empty drag" in {
    DroppedPaths.from(new FakeTransferable(Nil)) shouldBe Nil
  }

  it should "give nothing, rather than throw, when the drag cannot hand its data over" in {
    val failing = new FakeTransferable(List(DataFlavor.javaFileListFlavor -> (() => throw new IOException("gone"))))

    DroppedPaths.from(failing) shouldBe Nil
  }

  it should "fall back to the URI list when the file list is empty" in {
    val drag = new FakeTransferable(List(fileList(), uriText(uriListAsString, s"${notes.toUri}\n")))

    DroppedPaths.from(drag) shouldBe List(notes)
  }

  it should "ignore a file-list payload that is not a list of files" in {
    val drag = new FakeTransferable(List(DataFlavor.javaFileListFlavor -> (() => "not a list")))

    DroppedPaths.from(drag) shouldBe Nil
  }
