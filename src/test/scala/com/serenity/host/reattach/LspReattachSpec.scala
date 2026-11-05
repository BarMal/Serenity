package com.serenity.host.reattach

import com.serenity.host.reattach.ReattachFixtures.*
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LspReattachSpec extends AnyFlatSpec with Matchers:

  private val RequestCancelled = -32800

  private def toServer(actions: List[ReattachAction]): List[Json] =
    actions.collect { case ReattachAction.ToServer(message) => message }

  private def toClient(actions: List[ReattachAction]): List[Json] =
    actions.collect { case ReattachAction.ToClient(message) => message }

  private def idOf(message: Json): Json = message.hcursor.downField("id").focus.getOrElse(Json.Null)

  private def methodOf(message: Json): Option[String] = message.hcursor.get[String]("method").toOption

  private def uriOf(message: Json): Option[String] =
    message.hcursor.downField("params").downField("textDocument").get[String]("uri").toOption

  /** A client attached to a server that has completed the handshake with `initializeParams()`. */
  private def initialised(params: Json = initializeParams()): ReattachState =
    val (afterRequest, sent) = LspReattach.fromClient(ReattachState.fresh, request(num(1), "initialize", params))
    val (afterResult, _) =
      LspReattach.fromServer(afterRequest, response(idOf(toServer(sent).head), Json.obj("caps" -> true.asJson)))
    LspReattach.fromClient(afterResult, notification("initialized"))._1

  private def feed(state: ReattachState, steps: (ReattachState => (ReattachState, List[ReattachAction]))*) =
    steps.foldLeft((state, List.empty[ReattachAction])) {
      case ((current, collected), step) =>
        val (next, actions) = step(current)
        (next, collected ++ actions)
    }

  private def fromClient(message: Json): ReattachState => (ReattachState, List[ReattachAction]) =
    LspReattach.fromClient(_, message)

  private def fromServer(message: Json): ReattachState => (ReattachState, List[ReattachAction]) =
    LspReattach.fromServer(_, message)

  "LspReattach" should "forward the first initialize to the server and relay its answer under the client's id" in {
    val (afterRequest, sent) =
      LspReattach.fromClient(ReattachState.fresh, request(str("c-1"), "initialize", initializeParams()))
    val serverMessage = toServer(sent).head

    methodOf(serverMessage) shouldBe Some("initialize")

    val (_, relayed) =
      LspReattach.fromServer(afterRequest, response(idOf(serverMessage), Json.obj("caps" -> true.asJson)))

    toClient(relayed) shouldBe List(response(str("c-1"), Json.obj("caps" -> true.asJson)))
  }

  it should "answer a later client's initialize from the cache and swallow its initialized" in {
    val attached        = initialised(initializeParams(processId = 100))
    val (detached, _)   = LspReattach.detach(attached)
    val (reattached, _) = LspReattach.attach(detached)
    val (asked, answered) =
      LspReattach.fromClient(reattached, request(num(7), "initialize", initializeParams(processId = 555)))

    toServer(answered) shouldBe Nil
    toClient(answered) shouldBe List(response(num(7), Json.obj("caps" -> true.asJson)))

    val (_, afterInitialized) = LspReattach.fromClient(asked, notification("initialized"))
    afterInitialized shouldBe Nil
  }

  it should "forward a later initialized to the server once the swallowed one is used up" in {
    val attached      = initialised()
    val (detached, _) = LspReattach.detach(attached)
    val (state, _) = feed(
      LspReattach.attach(detached)._1,
      fromClient(request(num(2), "initialize", initializeParams())),
      fromClient(notification("initialized"))
    )

    val (_, actions) = LspReattach.fromClient(state, notification("initialized"))

    toServer(actions).flatMap(methodOf) shouldBe List("initialized")
  }

  it should "restart the server and forward the initialize when the client's capabilities changed" in {
    val attached      = initialised(initializeParams(capabilities = Json.obj("a" -> 1.asJson)))
    val (detached, _) = LspReattach.detach(attached)
    val changed       = initializeParams(capabilities = Json.obj("a" -> 2.asJson))

    val (state, actions) =
      LspReattach.fromClient(LspReattach.attach(detached)._1, request(num(3), "initialize", changed))

    actions.headOption shouldBe Some(ReattachAction.RestartServer)
    toServer(actions).flatMap(methodOf) shouldBe List("initialize")
    state.hasCachedInitialize shouldBe false
  }

  it should "send didClose for every open document at detach, in a stable order, and none for a closed one" in {
    val (opened, _) = feed(
      initialised(),
      fromClient(didOpen("file:///b.scala")),
      fromClient(didOpen("file:///a.scala")),
      fromClient(didOpen("file:///c.scala")),
      fromClient(didClose("file:///c.scala"))
    )

    val (afterDetach, actions) = LspReattach.detach(opened)

    toServer(actions).map(message => (methodOf(message), uriOf(message))) shouldBe List(
      (Some("textDocument/didClose"), Some("file:///a.scala")),
      (Some("textDocument/didClose"), Some("file:///b.scala"))
    )
    LspReattach.detach(afterDetach)._2 shouldBe Nil
  }

  it should "forward a didOpen after reattaching, since the server holds no document" in {
    val (opened, _)    = feed(initialised(), fromClient(didOpen("file:///a.scala")))
    val (detached, _)  = LspReattach.detach(opened)
    val (_, forwarded) = LspReattach.fromClient(LspReattach.attach(detached)._1, didOpen("file:///a.scala"))

    toServer(forwarded) shouldBe List(didOpen("file:///a.scala"))
  }

  it should "give the server its own request ids and restore the client's on the answer" in {
    val state         = initialised()
    val (asked, sent) = LspReattach.fromClient(state, request(str("hover-1"), "textDocument/hover"))
    val serverId      = idOf(toServer(sent).head)
    val (_, relayed)  = LspReattach.fromServer(asked, response(serverId, Json.obj("contents" -> "x".asJson)))

    serverId should not be str("hover-1")
    toClient(relayed) shouldBe List(response(str("hover-1"), Json.obj("contents" -> "x".asJson)))
  }

  it should "never reuse a server-side id for two requests" in {
    val (_, sent) = feed(
      initialised(),
      fromClient(request(num(1), "textDocument/hover")),
      fromClient(request(num(1), "textDocument/definition"))
    )

    toServer(sent).map(idOf).distinct should have size 2
  }

  it should "drop the answer to a request an earlier client made" in {
    val (asked, sent) = LspReattach.fromClient(initialised(), request(num(5), "textDocument/hover"))
    val serverId      = idOf(toServer(sent).head)
    val (detached, _) = LspReattach.detach(asked)
    val reattached    = LspReattach.attach(detached)._1

    LspReattach.fromServer(reattached, response(serverId, Json.Null))._2 shouldBe Nil
  }

  it should "map a cancel request to the server's id for it" in {
    val (asked, sent) = LspReattach.fromClient(initialised(), request(num(9), "textDocument/completion"))
    val serverId      = idOf(toServer(sent).head)

    val (_, cancelled) = LspReattach.fromClient(asked, notification("$/cancelRequest", Json.obj("id" -> num(9))))

    toServer(cancelled) shouldBe List(notification("$/cancelRequest", Json.obj("id" -> serverId)))
  }

  it should "drop a cancel request for an id it does not know" in {
    LspReattach.fromClient(initialised(), notification("$/cancelRequest", Json.obj("id" -> num(42))))._2 shouldBe Nil
  }

  it should "pass an ordinary server notification through to the client while attached" in {
    val diagnostics = notification("textDocument/publishDiagnostics", Json.obj("uri" -> "file:///a.scala".asJson))

    toClient(LspReattach.fromServer(initialised(), diagnostics)._2) shouldBe List(diagnostics)
  }

  it should "drop server notifications while no client is attached" in {
    val (detached, _) = LspReattach.detach(initialised())

    LspReattach.fromServer(detached, notification("window/logMessage"))._2 shouldBe Nil
  }

  it should "answer a server request that is still waiting for the client with RequestCancelled at detach" in {
    val (waiting, _)  = LspReattach.fromServer(initialised(), request(num(31), "workspace/configuration"))
    val (_, onDetach) = LspReattach.detach(waiting)

    toServer(onDetach) shouldBe List(errorResponse(num(31), RequestCancelled))
  }

  it should "not cancel a server request the client has already answered" in {
    val (answered, _) = feed(
      initialised(),
      fromServer(request(num(31), "workspace/configuration")),
      fromClient(response(num(31), Json.arr()))
    )

    LspReattach.detach(answered)._2 shouldBe Nil
  }

  it should "drop a client's late answer to a server request after detach" in {
    val (waiting, _)  = LspReattach.fromServer(initialised(), request(num(31), "workspace/configuration"))
    val (detached, _) = LspReattach.detach(waiting)

    LspReattach.fromClient(LspReattach.attach(detached)._1, response(num(31), Json.arr()))._2 shouldBe Nil
  }

  it should "replay the dynamic registrations to a new client and swallow its answer" in {
    val register = registerCapability(
      num(11),
      registration("watch", "workspace/didChangeWatchedFiles"),
      registration("fmt", "textDocument/formatting")
    )
    val (registered, _) = feed(initialised(), fromServer(register), fromClient(response(num(11), Json.Null)))
    val (detached, _)   = LspReattach.detach(registered)

    val (reattached, replay) = LspReattach.attach(detached)
    val replayed             = toClient(replay).head

    methodOf(replayed) shouldBe Some("client/registerCapability")
    replayed.hcursor.downField("params").downField("registrations").values.map(_.size) shouldBe Some(2)
    LspReattach.fromClient(reattached, response(idOf(replayed), Json.Null))._2 shouldBe Nil
  }

  it should "stop replaying a registration the server withdrew" in {
    val withdraw = request(
      num(12),
      "client/unregisterCapability",
      Json.obj(
        "unregisterations" -> Json.arr(
          Json.obj("id" -> "watch".asJson, "method" -> "workspace/didChangeWatchedFiles".asJson)
        )
      )
    )
    val (state, _) = feed(
      initialised(),
      fromServer(registerCapability(num(11), registration("watch", "workspace/didChangeWatchedFiles"))),
      fromServer(withdraw)
    )

    toClient(LspReattach.attach(LspReattach.detach(state)._1)._2) shouldBe Nil
  }

  it should "replay a progress that is still running, with its latest message, and forget one that ended" in {
    val (state, _) = feed(
      initialised(),
      fromServer(progressCreate(num(20), "index")),
      fromServer(progress("index", progressValue("begin", "title" -> str("Indexing")))),
      fromServer(progress("index", progressValue("report", "message" -> str("40%")))),
      fromServer(progressCreate(num(21), "compile")),
      fromServer(progress("compile", progressValue("begin", "title" -> str("Compiling")))),
      fromServer(progress("compile", progressValue("end")))
    )

    val replay = toClient(LspReattach.attach(LspReattach.detach(state)._1)._2)

    replay.flatMap(methodOf) shouldBe List("window/workDoneProgress/create", "$/progress")
    replay.lastOption shouldBe Some(
      progress("index", progressValue("begin", "title" -> str("Indexing"), "message" -> str("40%")))
    )
  }

  it should "track progress that arrives while no client is attached" in {
    val (detached, _) = LspReattach.detach(initialised())
    val (state, _) = feed(
      detached,
      fromServer(progressCreate(num(20), "index")),
      fromServer(progress("index", progressValue("begin", "title" -> str("Indexing"))))
    )

    toClient(LspReattach.attach(state)._2).flatMap(methodOf) shouldBe List(
      "window/workDoneProgress/create",
      "$/progress"
    )
  }
