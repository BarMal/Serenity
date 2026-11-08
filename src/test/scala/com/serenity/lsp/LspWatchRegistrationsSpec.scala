package com.serenity.lsp

import java.nio.file.Path

import com.serenity.lsp.client.LspWatch.{FileChangeType, FileSystemWatcher, WatchedFileChange, WatchedGlob}
import com.serenity.lsp.client.{LspGlob, LspWatch, LspWatchRegistrations}
import io.circe.Json
import io.circe.parser.parse
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** A server's `workspace/didChangeWatchedFiles` registrations (#2531 W1): which file events it asked for, and the
  * merged, ordered events it is sent.
  */
class LspWatchRegistrationsSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:
  import FileChangeType.{Changed, Created, Deleted}

  private val root = Path.of("/work/project").toAbsolutePath

  private def file(relative: String): Path = root.resolve(relative)

  private def everything(glob: String): FileSystemWatcher =
    FileSystemWatcher(WatchedGlob.Anywhere(LspGlob.compile(glob)), FileChangeType.values.toSet)

  "coalesce" should "merge the events of one path in the order they happened" in {
    val a = file("a.scala")
    val cases = List(
      Vector(Created, Changed)          -> Vector(Created),
      Vector(Created, Deleted)          -> Vector(),
      Vector(Deleted, Created)          -> Vector(Changed),
      Vector(Changed, Deleted)          -> Vector(Deleted),
      Vector(Changed, Changed)          -> Vector(Changed),
      Vector(Created, Deleted, Created) -> Vector(Created)
    )
    cases.foreach { (events, expected) =>
      withClue(s"$events: ")(
        LspWatchRegistrations.coalesce(events.map(WatchedFileChange(a, _))).map(_.change) shouldBe expected
      )
    }
  }

  it should "keep each path at the position of its first event" in {
    val (a, b) = (file("a"), file("b"))
    LspWatchRegistrations.coalesce(
      Vector(WatchedFileChange(b, Changed), WatchedFileChange(a, Created), WatchedFileChange(b, Deleted))
    ) shouldBe Vector(WatchedFileChange(b, Deleted), WatchedFileChange(a, Created))
  }

  private val changes: Gen[Vector[WatchedFileChange]] =
    Gen.containerOf[Vector, WatchedFileChange](
      Gen.zip(Gen.oneOf("a", "b", "c").map(file), Gen.oneOf(FileChangeType.values.toSeq)).map(WatchedFileChange(_, _))
    )

  it should "be idempotent and leave at most one event per path" in
    forAll(changes) { events =>
      val once = LspWatchRegistrations.coalesce(events)
      LspWatchRegistrations.coalesce(once) shouldBe once
      once.map(_.path).distinct.size shouldBe once.size
    }

  "select" should "send nothing to a server that registered nothing" in {
    LspWatchRegistrations.empty.select(Vector(WatchedFileChange(file("a.scala"), Changed)), root) shouldBe empty
  }

  it should "send only the events whose path a registered glob matches" in {
    val registrations = LspWatchRegistrations.empty.register("r1", List(everything("**/*.scala")))
    val events = Vector(WatchedFileChange(file("src/A.scala"), Changed), WatchedFileChange(file("README"), Changed))
    registrations.select(events, root) shouldBe Vector(WatchedFileChange(file("src/A.scala"), Changed))
  }

  it should "send only the kinds a watcher asked for" in {
    val deletesOnly   = FileSystemWatcher(WatchedGlob.Anywhere(LspGlob.compile("**/*.scala")), Set(Deleted))
    val registrations = LspWatchRegistrations.empty.register("r1", List(deletesOnly))
    val events        = Vector(WatchedFileChange(file("A.scala"), Changed), WatchedFileChange(file("B.scala"), Deleted))
    registrations.select(events, root) shouldBe Vector(WatchedFileChange(file("B.scala"), Deleted))
  }

  it should "match a relative pattern only under its base" in {
    val underSrc      = FileSystemWatcher(WatchedGlob.Under(file("src"), LspGlob.compile("*.scala")), Set(Changed))
    val registrations = LspWatchRegistrations.empty.register("r1", List(underSrc))
    val events =
      Vector(WatchedFileChange(file("src/A.scala"), Changed), WatchedFileChange(file("test/A.scala"), Changed))
    registrations.select(events, root) shouldBe Vector(WatchedFileChange(file("src/A.scala"), Changed))
  }

  it should "match a plain pattern against the absolute path too, for servers that send one" in {
    val absolute      = s"${root.toString.replace('\\', '/')}/build.sbt"
    val registrations = LspWatchRegistrations.empty.register("r1", List(everything(absolute)))
    registrations.select(Vector(WatchedFileChange(file("build.sbt"), Changed)), root) should have size 1
  }

  it should "stop sending once a registration is withdrawn" in {
    val registrations =
      LspWatchRegistrations.empty.register("r1", List(everything("**/*"))).unregister("r1")
    registrations.select(Vector(WatchedFileChange(file("a"), Changed)), root) shouldBe empty
    registrations.isEmpty shouldBe true
  }

  "parseRegistrations" should "read the watchers of a didChangeWatchedFiles registration and skip other methods" in {
    val params = parse(s"""{"registrations": [
      {"id": "w1", "method": "workspace/didChangeWatchedFiles", "registerOptions": {"watchers": [
        {"globPattern": "**/*.scala"},
        {"globPattern": {"baseUri": "${root.resolve("src").toUri}", "pattern": "*.java"}, "kind": 4},
        {"globPattern": {"baseUri": {"uri": "${root.toUri}", "name": "p"}, "pattern": "build.sbt"}, "kind": 3}
      ]}},
      {"id": "f1", "method": "textDocument/formatting", "registerOptions": {}}
    ]}""").fold(error => fail(error.message), identity)

    val parsed   = LspWatch.parseRegistrations(params)
    val watchers = parsed.flatMap(_._2)
    parsed.map(_._1) shouldBe List("w1")
    watchers.map(_.kinds) shouldBe List(FileChangeType.values.toSet, Set(Deleted), Set(Created, Changed))
    watchers.map(_.glob) shouldBe List(
      WatchedGlob.Anywhere(LspGlob.compile("**/*.scala")),
      WatchedGlob.Under(file("src"), LspGlob.compile("*.java")),
      WatchedGlob.Under(root, LspGlob.compile("build.sbt"))
    )
  }

  it should "skip a watcher it cannot read and keep the rest" in {
    val params = parse("""{"registrations": [{"id": "w1", "method": "workspace/didChangeWatchedFiles",
      "registerOptions": {"watchers": [{"globPattern": 42}, {"globPattern": "*.ts"}]}}]}""").fold(
      error => fail(error.message),
      identity
    )
    LspWatch.parseRegistrations(params).flatMap(_._2).map(_.glob) shouldBe
      List(WatchedGlob.Anywhere(LspGlob.compile("*.ts")))
  }

  "parseUnregistrations" should "read the ids of withdrawn didChangeWatchedFiles registrations" in {
    // The spec's own field name is misspelled `unregisterations`, and servers send it that way.
    val params = parse("""{"unregisterations": [
      {"id": "w1", "method": "workspace/didChangeWatchedFiles"},
      {"id": "f1", "method": "textDocument/formatting"}
    ]}""").fold(error => fail(error.message), identity)
    LspWatch.parseUnregistrations(params) shouldBe List("w1")
  }

  "didChangeWatchedFilesParams" should "encode each change as a file URI with its LSP type number" in {
    val json = LspWatch.didChangeWatchedFilesParams(
      Vector(WatchedFileChange(file("a"), Created), WatchedFileChange(file("b"), Deleted))
    )
    json shouldBe Json.obj(
      "changes" -> Json.arr(
        Json.obj("uri" -> Json.fromString(file("a").toUri.toString), "type" -> Json.fromInt(1)),
        Json.obj("uri" -> Json.fromString(file("b").toUri.toString), "type" -> Json.fromInt(3))
      )
    )
  }
