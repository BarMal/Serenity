package com.serenity.io

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Objective-C message sequence behind macOS's Open..., checked against a recording [[ObjectiveC]]. The calls into
  * the real runtime (`JnaObjectiveC`) cannot run without a Mac; everything that decides what is sent is here.
  */
class OpenPanelScriptSpec extends AnyFlatSpec with Matchers:

  private val pool   = 1L
  private val panel  = 2L
  private val string = 3L
  private val url    = 4L

  private val allocatedPool = 11L
  private val livePool      = 12L
  private val livePanel     = 20L
  private val cString       = 30L
  private val nsString      = 31L
  private val directoryUrl  = 40L
  private val chosenUrl     = 50L
  private val chosenPath    = 51L
  private val chosenBytes   = 52L

  final private case class Message(receiver: Long, selector: String, arguments: List[Long])

  final private class RecordingObjectiveC(
      modalResponse: Long = 1L,
      chosenUrlId: Long = chosenUrl,
      missingClass: Option[String] = None,
      failOn: Option[String] = None
  ) extends ObjectiveC:
    private val log  = Ref.unsafe[IO, Vector[Message]](Vector.empty)
    private val utf8 = Ref.unsafe[IO, Vector[String]](Vector.empty)

    def messages: List[Message] = log.get.unsafeRunSync().toList
    def encoded: List[String]   = utf8.get.unsafeRunSync().toList

    def classNamed(name: String): Long =
      if missingClass.contains(name) then 0L
      else
        name match
          case "NSAutoreleasePool" => pool
          case "NSOpenPanel"       => panel
          case "NSString"          => string
          case "NSURL"             => url
          case _                   => 0L

    def send(receiver: Long, selector: String, arguments: Long*): Long =
      log.update(_ :+ Message(receiver, selector, arguments.toList)).unsafeRunSync()
      if failOn.contains(selector) then throw new IllegalStateException(s"$selector exploded")
      (receiver, selector) match
        case (`pool`, "alloc")                       => allocatedPool
        case (`allocatedPool`, "init")               => livePool
        case (`panel`, "openPanel")                  => livePanel
        case (`string`, "stringWithUTF8String:")     => nsString
        case (`url`, "fileURLWithPath:isDirectory:") => directoryUrl
        case (`livePanel`, "runModal")               => modalResponse
        case (`livePanel`, "URL")                    => chosenUrlId
        case (`chosenUrl`, "path")                   => chosenPath
        case (`chosenPath`, "UTF8String")            => chosenBytes
        case _                                       => 0L

    def withUtf8[A](text: String)(use: Long => A): A =
      utf8.update(_ :+ text).unsafeRunSync()
      use(cString)

    def readUtf8(pointer: Long): Option[String] =
      Option.when(pointer == chosenBytes)("/Users/ada/notes/draft.md")

  private val configuration = List(
    Message(livePanel, "setCanChooseFiles:", List(1L)),
    Message(livePanel, "setCanChooseDirectories:", List(1L)),
    Message(livePanel, "setAllowsMultipleSelection:", List(0L))
  )

  private val poolLifecycle = (
    List(Message(pool, "alloc", Nil), Message(allocatedPool, "init", Nil)),
    Message(livePool, "drain", Nil)
  )

  "The open panel script" should "accept files and directories, one selection, and read the chosen path" in {
    val objc = new RecordingObjectiveC()

    val result = OpenPanelScript.run(objc, None)

    result shouldBe Right(Some(Path.of("/Users/ada/notes/draft.md")))
    objc.messages shouldBe poolLifecycle._1 ++ List(Message(panel, "openPanel", Nil)) ++ configuration ++ List(
      Message(livePanel, "runModal", Nil),
      Message(livePanel, "URL", Nil),
      Message(chosenUrl, "path", Nil),
      Message(chosenPath, "UTF8String", Nil),
      poolLifecycle._2
    )
  }

  it should "start in the directory it is given" in {
    val directory = TestTemp.directory("serenity-open-panel")
    val objc      = new RecordingObjectiveC()

    OpenPanelScript.run(objc, Some(directory))

    objc.encoded shouldBe List(directory.toAbsolutePath.normalize().toString)
    objc.messages.slice(6, 9) shouldBe List(
      Message(string, "stringWithUTF8String:", List(cString)),
      Message(url, "fileURLWithPath:isDirectory:", List(nsString, 1L)),
      Message(livePanel, "setDirectoryURL:", List(directoryUrl))
    )
    Files.deleteIfExists(directory)
  }

  it should "leave the panel's own starting directory alone for a path that is not an existing directory" in {
    val file = TestTemp.file("serenity-open-panel", ".txt")

    for initial <- List(Some(file), Some(file.resolveSibling("does-not-exist")), None) do
      val objc = new RecordingObjectiveC()
      OpenPanelScript.run(objc, initial)
      objc.messages.map(_.selector) should not contain "setDirectoryURL:"
      objc.encoded shouldBe empty
    Files.deleteIfExists(file)
  }

  it should "report cancellation without reading a selection, and still drain the pool" in {
    for response <- List(0L, -1000L, 2L) do
      val objc = new RecordingObjectiveC(modalResponse = response)

      OpenPanelScript.run(objc, None) shouldBe Right(None)

      objc.messages.map(_.selector) should not contain "URL"
      objc.messages.lastOption shouldBe Some(poolLifecycle._2)
  }

  it should "fail, and still drain the pool, when the panel answers OK without a URL" in {
    val objc = new RecordingObjectiveC(chosenUrlId = 0L)

    OpenPanelScript.run(objc, None).left.map(_.contains("URL")) shouldBe Left(true)
    objc.messages.lastOption shouldBe Some(poolLifecycle._2)
  }

  it should "fail without sending anything when a class is missing" in {
    for missing <- List("NSOpenPanel", "NSAutoreleasePool", "NSString", "NSURL") do
      val objc = new RecordingObjectiveC(missingClass = Some(missing))

      OpenPanelScript.run(objc, None).left.map(_.contains(missing)) shouldBe Left(true)
      objc.messages shouldBe empty
  }

  it should "turn a failure raised by the runtime into a Left, and still drain the pool" in {
    val objc = new RecordingObjectiveC(failOn = Some("runModal"))

    OpenPanelScript.run(objc, None).left.map(_.contains("runModal exploded")) shouldBe Left(true)
    objc.messages.lastOption shouldBe Some(poolLifecycle._2)
  }

  "NSModalResponse" should "count only NSModalResponseOK as a choice" in {
    OpenPanelScript.chose(1L) shouldBe true
    List(0L, -1000L, -1001L, 2L, 1000L).foreach(OpenPanelScript.chose(_) shouldBe false)
  }
