package com.serenity.input

import java.awt.datatransfer.DataFlavor
import java.nio.file.{Path, Paths}

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2388: the decisions behind the editor's drop target, away from AWT's drag events. */
class FileDropHandlerSpec extends AnyFlatSpec with Matchers:

  private val uriList = new DataFlavor("text/uri-list;class=java.lang.String")
  private val notes   = Paths.get(System.getProperty("java.io.tmpdir")).resolve("notes.md")

  final private class Recorder:
    private val dropped: Ref[IO, List[List[Path]]] = Ref.unsafe(Nil)
    private val offered: Ref[IO, List[String]]     = Ref.unsafe(Nil)

    val handler: FileDropHandler =
      FileDropHandler(
        onDropped = paths => dropped.update(_ :+ paths).unsafeRunSync(),
        onOffered = description => offered.update(_ :+ description).unsafeRunSync()
      )

    def droppedBatches: List[List[Path]] = dropped.get.unsafeRunSync()
    def offerLog: List[String]           = offered.get.unsafeRunSync()

  private def fileDrag: FakeTransferable =
    new FakeTransferable(List(DataFlavor.javaFileListFlavor -> (() => List(notes.toFile).asJava)))

  "accepts" should "welcome a drag that offers a file list" in {
    Recorder().handler.accepts(Array(DataFlavor.javaFileListFlavor)) shouldBe true
  }

  it should "welcome a drag that offers only a URI list, whatever class it comes as" in {
    val asStream = new DataFlavor("text/uri-list;class=java.io.InputStream")

    Recorder().handler.accepts(Array(uriList)) shouldBe true
    Recorder().handler.accepts(Array(asStream)) shouldBe true
  }

  it should "refuse a drag that offers only text" in {
    Recorder().handler.accepts(Array(DataFlavor.stringFlavor)) shouldBe false
  }

  it should "refuse a drag that offers nothing" in {
    Recorder().handler.accepts(Array.empty[DataFlavor]) shouldBe false
  }

  "handle" should "pass the dropped paths on once and report the drop as taken" in {
    val recorder = Recorder()

    recorder.handler.handle(fileDrag) shouldBe true
    recorder.droppedBatches shouldBe List(List(notes))
  }

  it should "report a drop with no usable path as not taken, and pass nothing on" in {
    val recorder = Recorder()
    val textOnly = new FakeTransferable(List(DataFlavor.stringFlavor -> (() => "text")))

    recorder.handler.handle(textOnly) shouldBe false
    recorder.droppedBatches shouldBe Nil
  }

  "entered" should "log what the drag offers, once, and say whether it is welcome" in {
    val recorder = Recorder()

    val welcome = recorder.handler.entered(Array(DataFlavor.javaFileListFlavor, uriList))

    welcome shouldBe true
    recorder.offerLog should have size 1
    recorder.offerLog.head should include(DataFlavor.javaFileListFlavor.getMimeType)
    recorder.offerLog.head should include("text/uri-list")
  }

  it should "log a drag that offers nothing, and refuse it" in {
    val recorder = Recorder()

    recorder.handler.entered(Array.empty[DataFlavor]) shouldBe false
    recorder.offerLog should have size 1
    recorder.offerLog.head should include("no flavors")
  }

  it should "log a refused drag too, so a no-op on someone's setup can be diagnosed" in {
    val recorder = Recorder()

    recorder.handler.entered(Array(DataFlavor.stringFlavor)) shouldBe false
    recorder.offerLog.head should include(DataFlavor.stringFlavor.getMimeType)
  }
