package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import com.serenity.lsp.client.{
  DocumentUri,
  LspApplyEditRequest,
  LspApplyEditResult,
  LspClientHooks,
  LspConnection,
  LspMessageAction,
  LspMessageLevel,
  LspMessageRequest,
  LspProgressUpdate,
  LspServerMessage,
  WorkspaceRootUri
}
import com.serenity.lsp.config.LanguageId
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Requests a server sends the client (#1847), exercised over the same in-memory pipes a real server process would use:
  * every one must get exactly one JSON-RPC reply carrying its id back in the form it arrived in.
  */
class LspServerRequestSpec extends AnyFlatSpec with Matchers:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val logger      = LoggerFactory[IO].getLogger(using LoggerName("LspServerRequestSpec"))
  private val testTimeout = 5.seconds

  private val handshakeDone: Resource[IO, MockLspServer] =
    for
      server <- MockLspServer.resource(Map("initialize" -> Json.obj("capabilities" -> Json.obj())), logger)
      _ <- LspConnection.connect(
        LanguageId.Scala,
        server.clientIn,
        server.clientOut,
        WorkspaceRootUri("file:///workspace"),
        logger
      )
      _ <- Resource.eval(server.drainReceived(2))
    yield server

  private def replyTo(id: Json, method: String, params: Json): Json =
    handshakeDone
      .use { server =>
        server.push(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "method" -> method.asJson, "params" -> params)) >>
          server.takeReceived
      }
      .timeout(testTimeout)
      .unsafeRunSync()

  private def result(reply: Json): Option[Json] = reply.hcursor.downField("result").focus

  "A server request" should "be answered with one null per requested workspace/configuration item" in {
    val reply = replyTo(
      7.asJson,
      "workspace/configuration",
      Json.obj("items" -> Json.arr(Json.obj("section" -> "metals".asJson), Json.obj("scopeUri" -> "file:///a".asJson)))
    )

    reply.hcursor.downField("jsonrpc").as[String].toOption shouldBe Some("2.0")
    reply.hcursor.downField("id").as[Long].toOption shouldBe Some(7L)
    reply.hcursor.downField("method").succeeded shouldBe false
    result(reply) shouldBe Some(Json.arr(Json.Null, Json.Null))
  }

  it should "echo a string id back as a string" in {
    val reply = replyTo("configuration-1".asJson, "workspace/configuration", Json.obj("items" -> Json.arr()))

    reply.hcursor.downField("id").as[String].toOption shouldBe Some("configuration-1")
    result(reply) shouldBe Some(Json.arr())
  }

  it should "acknowledge client/registerCapability with a null result" in {
    val registration = Json.obj("id" -> "watch".asJson, "method" -> "workspace/didChangeWatchedFiles".asJson)
    val reply = replyTo(3.asJson, "client/registerCapability", Json.obj("registrations" -> Json.arr(registration)))

    reply.hcursor.downField("id").as[Long].toOption shouldBe Some(3L)
    result(reply) shouldBe Some(Json.Null)
    reply.hcursor.downField("error").succeeded shouldBe false
  }

  it should "acknowledge window/workDoneProgress/create with a null result" in {
    val reply = replyTo("progress".asJson, "window/workDoneProgress/create", Json.obj("token" -> "indexing".asJson))

    reply.hcursor.downField("id").as[String].toOption shouldBe Some("progress")
    result(reply) shouldBe Some(Json.Null)
  }

  it should "take the first offered action for window/showMessageRequest" in {
    val importBuild = Json.obj("title" -> "Import build".asJson)
    val reply = replyTo(
      11.asJson,
      "window/showMessageRequest",
      Json.obj(
        "type"    -> 3.asJson,
        "message" -> "New build detected".asJson,
        "actions" -> Json.arr(importBuild, Json.obj("title" -> "Not now".asJson))
      )
    )

    result(reply) shouldBe Some(importBuild)
  }

  it should "answer window/showMessageRequest without actions with null" in {
    val reply = replyTo(12.asJson, "window/showMessageRequest", Json.obj("type" -> 1.asJson, "message" -> "Hi".asJson))

    result(reply) shouldBe Some(Json.Null)
  }

  it should "refuse a method the client does not serve with MethodNotFound" in {
    val reply = replyTo(5.asJson, "custom/unknownRequest", Json.obj())

    reply.hcursor.downField("id").as[Long].toOption shouldBe Some(5L)
    reply.hcursor.downField("error").downField("code").as[Int].toOption shouldBe Some(-32601)
    result(reply) shouldBe None
  }

  private val edit = Json.obj(
    "newText" -> "x".asJson,
    "range" -> Json.obj(
      "start" -> Json.obj("line" -> 0.asJson, "character" -> 0.asJson),
      "end"   -> Json.obj("line" -> 0.asJson, "character" -> 1.asJson)
    )
  )

  private def withHooks[A](hooks: LspClientHooks)(use: (MockLspServer, LspConnection) => IO[A]): A =
    (for
      server <- MockLspServer.resource(Map("initialize" -> Json.obj("capabilities" -> Json.obj())), logger)
      connection <- LspConnection.connect(
        LanguageId.Scala,
        server.clientIn,
        server.clientOut,
        WorkspaceRootUri("file:///workspace"),
        logger,
        requestTimeout = 300.millis
      )
      _ <- Resource.eval(server.drainReceived(2))
      _ <- Resource.make(connection.processIncoming((_, _) => IO.unit, hooks).start)(_.cancel)
    yield (server, connection)).use(use.tupled).timeout(testTimeout).unsafeRunSync()

  it should "apply workspace/applyEdit through the client and report whether it was applied" in {
    val seen = new java.util.concurrent.atomic.AtomicReference[Option[LspApplyEditRequest]](None)
    val hooks = LspClientHooks.ignoring.copy(onApplyEdit =
      request => IO(seen.set(Some(request))).as(LspApplyEditResult(applied = true, None))
    )
    val params = Json.obj(
      "label" -> "Organise imports".asJson,
      "edit"  -> Json.obj("changes" -> Json.obj("file:///a" -> Json.arr(edit)))
    )

    val reply = withHooks(hooks) { (server, _) =>
      server.push(
        Json.obj(
          "jsonrpc" -> "2.0".asJson,
          "id"      -> 21.asJson,
          "method"  -> "workspace/applyEdit".asJson,
          "params"  -> params
        )
      ) >>
        server.takeReceived
    }

    result(reply).flatMap(_.hcursor.downField("applied").as[Boolean].toOption) shouldBe Some(true)
    reply.hcursor.downField("id").as[Long].toOption shouldBe Some(21L)
    seen.get.map(_.label) shouldBe Some(Some("Organise imports"))
    seen.get.map(_.edits.keySet) shouldBe Some(Set(DocumentUri("file:///a")))
  }

  it should "answer workspace/applyEdit as not applied when the client takes too long" in {
    val hooks = LspClientHooks.ignoring.copy(onApplyEdit = _ => IO.never)
    val reply = withHooks(hooks) { (server, _) =>
      server.push(
        Json.obj(
          "jsonrpc" -> "2.0".asJson,
          "id"      -> 22.asJson,
          "method"  -> "workspace/applyEdit".asJson,
          "params"  -> Json.obj("edit" -> Json.obj("changes" -> Json.obj("file:///a" -> Json.arr(edit))))
        )
      ) >> server.takeReceived
    }

    result(reply).flatMap(_.hcursor.downField("applied").as[Boolean].toOption) shouldBe Some(false)
  }

  it should "report window/showMessage and $/progress notifications to the client" in {
    val messages = new java.util.concurrent.LinkedBlockingQueue[LspServerMessage]()
    val updates  = new java.util.concurrent.LinkedBlockingQueue[LspProgressUpdate]()
    val hooks = LspClientHooks.ignoring.copy(
      onMessage = message => IO(messages.put(message)),
      onProgress = update => IO(updates.put(update))
    )

    withHooks(hooks) { (server, _) =>
      server.push(
        Json.obj(
          "jsonrpc" -> "2.0".asJson,
          "method"  -> "window/showMessage".asJson,
          "params"  -> Json.obj("type" -> 2.asJson, "message" -> "Heads up".asJson)
        )
      ) >>
        server.push(
          Json.obj(
            "jsonrpc" -> "2.0".asJson,
            "method"  -> "$/progress".asJson,
            "params" -> Json.obj(
              "token" -> "index".asJson,
              "value" -> Json.obj("kind" -> "begin".asJson, "title" -> "Indexing".asJson)
            )
          )
        ) >>
        IO.blocking((messages.take(), updates.take())).timeout(testTimeout)
    } shouldBe (
      LspServerMessage(LspMessageLevel.Warning, "Heads up"),
      LspProgressUpdate("index", com.serenity.lsp.model.LspProgress.Begin("Indexing", None, None))
    )
  }

  it should "pass a window/logMessage on as a log line, never as something to show the user" in {
    val messages = new java.util.concurrent.LinkedBlockingQueue[LspServerMessage]()
    val hooks    = LspClientHooks.ignoring.copy(onMessage = message => IO(messages.put(message)))

    withHooks(hooks) { (server, _) =>
      server.push(
        Json.obj(
          "jsonrpc" -> "2.0".asJson,
          "method"  -> "window/logMessage".asJson,
          "params"  -> Json.obj("type" -> 1.asJson, "message" -> "indexing failed".asJson)
        )
      ) >> IO.blocking(messages.take()).timeout(testTimeout)
    } shouldBe LspServerMessage(LspMessageLevel.Error, "indexing failed", shownToUser = false)
  }

  private val importBuild = Json.obj("title" -> "Import build".asJson, "kind" -> "import".asJson)
  private val notNow      = Json.obj("title" -> "Not now".asJson)

  private def messageRequest(id: Long): Json =
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id"      -> id.asJson,
      "method"  -> "window/showMessageRequest".asJson,
      "params" -> Json.obj(
        "type"    -> 3.asJson,
        "message" -> "New build detected".asJson,
        "actions" -> Json.arr(importBuild, notNow)
      )
    )

  it should "ask the client which action to take for window/showMessageRequest and answer with that item" in {
    val seen  = new java.util.concurrent.atomic.AtomicReference[Option[LspMessageRequest]](None)
    val hooks = LspClientHooks.ignoring.copy(onMessageRequest = request => IO(seen.set(Some(request))).as(Some(1)))

    val reply = withHooks(hooks)((server, _) => server.push(messageRequest(41)) >> server.takeReceived)

    result(reply) shouldBe Some(notNow)
    reply.hcursor.downField("id").as[Long].toOption shouldBe Some(41L)
    seen.get shouldBe Some(
      LspMessageRequest(
        LspMessageLevel.Info,
        "New build detected",
        List(LspMessageAction("Import build", importBuild), LspMessageAction("Not now", notNow))
      )
    )
  }

  it should "answer window/showMessageRequest with null when the client dismisses the question" in {
    val hooks = LspClientHooks.ignoring.copy(onMessageRequest = _ => IO.pure(None))

    val reply = withHooks(hooks)((server, _) => server.push(messageRequest(42)) >> server.takeReceived)

    result(reply) shouldBe Some(Json.Null)
  }

  it should "answer window/showMessageRequest with null for an action the client never offered" in {
    val hooks = LspClientHooks.ignoring.copy(onMessageRequest = _ => IO.pure(Some(7)))

    val reply = withHooks(hooks)((server, _) => server.push(messageRequest(43)) >> server.takeReceived)

    result(reply) shouldBe Some(Json.Null)
  }

  it should "give up on a window/showMessageRequest nobody answers, telling the client it is no longer wanted" in {
    val cancelled = new java.util.concurrent.atomic.AtomicBoolean(false)
    val hooks =
      LspClientHooks.ignoring.copy(onMessageRequest = _ => IO.never[Option[Int]].onCancel(IO(cancelled.set(true))))

    val reply = withHooks(hooks)((server, _) => server.push(messageRequest(44)) >> server.takeReceived)

    result(reply) shouldBe Some(Json.Null)
    cancelled.get shouldBe true
  }

  it should "send exactly one reply to a window/showMessageRequest, however the client ends" in {
    val hooks = LspClientHooks.ignoring.copy(onMessageRequest = _ => IO.raiseError(new RuntimeException("boom")))

    val replies = withHooks(hooks) { (server, _) =>
      server.push(messageRequest(45)) >> server.takeReceived.flatMap { first =>
        server.takeReceived.timeout(600.millis).attempt.map(second => (first, second.toOption))
      }
    }

    result(replies._1) shouldBe Some(Json.Null)
    replies._2 shouldBe None
  }
