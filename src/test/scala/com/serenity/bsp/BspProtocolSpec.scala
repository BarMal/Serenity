package com.serenity.bsp

import java.net.URI
import java.nio.file.Path

import scala.io.Source
import scala.util.Using

import io.circe.Json
import io.circe.parser.parse
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The BSP messages Serenity sends and reads (#2522 B1), checked against a session recorded from sbt 1.10.7's own BSP
  * server: initialize, buildTargets, compiles with and without an error, a run, shutdown and exit. Local paths in the
  * recording were replaced with `/workspace/hello` and `/cache/...`.
  */
class BspProtocolSpec extends AnyFlatSpec with Matchers:

  private final case class Recorded(outgoing: Boolean, message: Json)

  private val session: Vector[Recorded] =
    Using.resource(Source.fromResource("bsp/sbt-1.10.7-session.jsonl"))(_.getLines().toVector).map { line =>
      val json = parse(line).fold(error => fail(error.getMessage), identity)
      Recorded(json.hcursor.get[String]("dir").contains("out"), json.hcursor.downField("msg").focus.get)
    }

  private def received: Vector[Json] = session.filterNot(_.outgoing).map(_.message)
  private def sent: Vector[Json]     = session.filter(_.outgoing).map(_.message)

  private def method(json: Json): Option[String] = json.hcursor.get[String]("method").toOption

  private def firstReceived(withMethod: String): Json =
    received.find(json => method(json).contains(withMethod)).getOrElse(fail(s"no $withMethod in the recording"))

  private def resultOf(id: Int): Json =
    received
      .find(json => json.hcursor.get[Int]("id").contains(id) && json.hcursor.downField("result").succeeded)
      .flatMap(_.hcursor.downField("result").focus)
      .getOrElse(fail(s"no result for request $id"))

  "Reading a recorded sbt session" should "decode every message the server sent, ignoring the ones it does not know" in {
    val decoded = received.map(BspProtocol.inbound)

    all(decoded.map(_.isRight)) shouldBe true
    val inbound = decoded.collect { case Right(message) => message }
    inbound.count { case BspInbound.Response(_, _) => true; case _ => false } shouldBe 18
    inbound.collect { case BspInbound.Notification(BspNotification.Other(name)) => name } shouldBe Vector("shutdown")
  }

  it should "read the server's capabilities from the initialize result: no debugging without an adapter plugin" in {
    BspProtocol.initializeResult(resultOf(1)) shouldBe Right(
      InitializeResult(
        displayName = "sbt",
        version = "1.10.7",
        bspVersion = "2.1.0-M1",
        capabilities = ServerCapabilities(compile = true, test = true, run = true, debug = false, canReload = true)
      )
    )
  }

  it should "read the build targets with what each one can do" in {
    val targets = BspProtocol.buildTargets(resultOf(2)).fold(error => fail(error.getMessage), identity)

    targets.map(_.id.uri) should contain(BuildTargetId("file:/workspace/hello/#sbt-proj/Compile").uri)
    val main = targets.find(_.id == BuildTargetId("file:/workspace/hello/#sbt-proj/Compile")).get
    main.displayName shouldBe Some("sbt-proj")
    (main.canCompile, main.canTest, main.canRun, main.canDebug) shouldBe ((true, true, true, false))
  }

  it should "treat sbt's 'Compilation failed' error as a failed compile, not a broken connection" in {
    val error = received
      .find(_.hcursor.downField("error").succeeded)
      .flatMap(json => BspProtocol.inbound(json).toOption)
      .getOrElse(fail("no error response in the recording"))

    error match
      case BspInbound.Response(_, outcome) =>
        BspProtocol.compileOutcome(outcome) shouldBe CompileOutcome.Failed(
          "(Compile / compileIncremental) Compilation failed"
        )
      case other => fail(s"expected a response, got $other")
  }

  it should "map a compile result's status code, and any other error, to an outcome" in {
    def result(code: Int) = Right(Json.obj("statusCode" -> Json.fromInt(code)))
    BspProtocol.compileOutcome(result(1)) shouldBe CompileOutcome.Ok
    BspProtocol.compileOutcome(result(2)) shouldBe CompileOutcome.Failed("compile reported errors")
    BspProtocol.compileOutcome(result(3)) shouldBe CompileOutcome.Cancelled
    BspProtocol.compileOutcome(Left(RpcError(-32601, "method not found"))) shouldBe
      CompileOutcome.ServerError(-32601, "method not found")
  }

  it should "read a published error with its file, range, severity and reset flag" in {
    val published = received
      .filter(json => method(json).contains("build/publishDiagnostics"))
      .flatMap(json => BspProtocol.inbound(json).toOption)
      .collect { case BspInbound.Notification(BspNotification.Diagnostics(params)) => params }
      .find(_.diagnostics.nonEmpty)
      .getOrElse(fail("no diagnostics with an error in the recording"))

    published.document shouldBe Path.of("/workspace/hello/src/main/scala/Hello.scala")
    published.target shouldBe BuildTargetId("file:/workspace/hello/#sbt-proj/Compile")
    published.reset shouldBe true
    published.diagnostics.map(d => (d.message, d.severity, d.range)) shouldBe List(
      ("value nope is not a member of Int", Some(1), DiagnosticRange(0, 47, 0, 53))
    )
  }

  it should "read task start, progress and finish, and the run's output as log messages" in {
    val notifications = received.flatMap(json => BspProtocol.inbound(json).toOption).collect {
      case BspInbound.Notification(notification) => notification
    }

    notifications.collectFirst { case started: BspNotification.TaskStarted => started.message } shouldBe Some(
      "Compiling sbt-proj"
    )
    notifications.collectFirst { case progress: BspNotification.TaskProgressed => (progress.progress, progress.total) }
      .flatten2 shouldBe Some((10L, 30L))
    notifications.collectFirst { case finished: BspNotification.TaskFinished => finished.status } shouldBe Some(1)
    notifications.collect { case log: BspNotification.Log => log.message }.exists(_.startsWith("hi ")) shouldBe true
  }

  "A file URI from the server" should "name the same file whether written file:/ or file:///" in {
    BspProtocol.documentPath("file:/workspace/hello/A.scala") shouldBe Some(Path.of("/workspace/hello/A.scala"))
    BspProtocol.documentPath("file:///workspace/hello/A.scala") shouldBe Some(Path.of("/workspace/hello/A.scala"))
    BspProtocol.documentPath("jar:file:/x.jar!/A.class") shouldBe None
  }

  "Writing requests" should "produce exactly the messages the recorded session sent" in {
    val root = URI.create("file:///workspace/hello")
    val main = BuildTargetId("file:/workspace/hello/#sbt-proj/Compile")

    val written = Vector(
      BspProtocol.request(1, "build/initialize", BspProtocol.initializeParams(root, "serenity-spike", "0", List("scala", "java"))),
      BspProtocol.notification("build/initialized", Json.obj()),
      BspProtocol.request(2, "workspace/buildTargets", Json.obj()),
      BspProtocol.request(3, "buildTarget/compile", BspProtocol.compileParams(List(main), "o1")),
      BspProtocol.request(12, "buildTarget/run", BspProtocol.runParams(main, "run1")),
      BspProtocol.request(5, "build/shutdown", Json.Null),
      BspProtocol.notification("build/exit", Json.Null)
    )

    written.foreach(message => sent should contain(message))
  }

  extension [A, B](option: Option[(Option[A], Option[B])])
    private def flatten2: Option[(A, B)] = option.flatMap((a, b) => a.zip(b))
