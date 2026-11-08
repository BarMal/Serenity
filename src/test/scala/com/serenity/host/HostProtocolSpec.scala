package com.serenity.host

import java.time.Instant

import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HostProtocolSpec extends AnyFlatSpec with Matchers:

  private val identity = LaunchIdentity(List("metals"), "/work/serenity", "env-1", "init-1")

  private val record = HostRecord(
    pid = 4242,
    root = "/work/serenity",
    protocolVersion = 1,
    startedAt = Instant.parse("2026-11-03T09:00:00Z"),
    servers = List(
      HostedServer("scala", pid = 4300, ServerState.Running, identity),
      HostedServer("typescript", pid = 4301, ServerState.Dead(exitCode = Some(137)), identity)
    )
  )

  private val requests: List[HostRequest] = List(
    HostRequest.Hello(token = "t0ken", protocolVersion = 1, clientPid = 99),
    HostRequest.Status,
    HostRequest.Detach,
    HostRequest.Stop,
    HostRequest.RestartServer("scala")
  )

  private val responses: List[HostResponse] = List(
    HostResponse.Welcome(record),
    HostResponse.Busy(heldByPid = 77),
    HostResponse.Refused(Refusal.BadToken),
    HostResponse.Refused(Refusal.UnsupportedProtocol(supported = 1)),
    HostResponse.Refused(Refusal.UnknownServer("go")),
    HostResponse.Ok,
    HostResponse.Report(record)
  )

  "HostRequest" should "round-trip every message through one line of JSON" in
    requests.foreach { request =>
      val line = HostProtocol.encode(request)

      line should not include "\n"
      HostProtocol.decodeRequest(line) shouldBe Right(request)
    }

  it should "refuse a message of an unknown type, so the host can close the connection" in {
    HostProtocol.decodeRequest("""{"type":"reboot"}""").isLeft shouldBe true
  }

  it should "refuse text that is not JSON, and a hello without a protocol version" in {
    HostProtocol.decodeRequest("not json").isLeft shouldBe true
    HostProtocol.decodeRequest("""{"type":"hello","token":"x","clientPid":1}""").isLeft shouldBe true
  }

  it should "ignore fields it does not know, so a newer client can add them" in {
    HostProtocol.decodeRequest("""{"type":"status","future":true}""") shouldBe Right(HostRequest.Status)
  }

  "HostResponse" should "round-trip every message through one line of JSON" in
    responses.foreach { response =>
      val line = HostProtocol.encode(response)

      line should not include "\n"
      HostProtocol.decodeResponse(line) shouldBe Right(response)
    }

  "HostRecord" should "round-trip as JSON and never carry the token" in {
    val json = record.asJson

    json.as[HostRecord] shouldBe Right(record)
    json.noSpaces.toLowerCase should not include "token"
  }

  it should "write its start time as an ISO-8601 instant" in {
    parse(record.asJson.noSpaces).flatMap(_.hcursor.get[String]("startedAt")) shouldBe Right("2026-11-03T09:00:00Z")
  }

  it should "describe a dead server by its exit code, or by none when it is unknown" in {
    val dead = HostedServer("go", 1, ServerState.Dead(None), identity)

    dead.asJson.as[HostedServer] shouldBe Right(dead)
  }

  it should "reject a record of the wrong shape" in {
    Json.obj("pid" -> Json.fromString("x")).as[HostRecord].isLeft shouldBe true
  }
