package com.serenity.host

import java.time.Instant

import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.*
import io.circe.{Codec, Decoder, Encoder, Json}

enum ServerState:
  case Running
  case Dead(exitCode: Option[Int])

object ServerState:

  given Encoder[ServerState] = Encoder.instance:
    case Running        => Json.obj("state" -> "running".asJson)
    case Dead(exitCode) => Json.obj("state" -> "dead".asJson, "exitCode" -> exitCode.asJson)

  given Decoder[ServerState] = Decoder.instance: cursor =>
    cursor
      .get[String]("state")
      .flatMap:
        case "running" => Right(Running)
        case "dead"    => cursor.get[Option[Int]]("exitCode").map(Dead(_))
        case other     => Left(io.circe.DecodingFailure(s"unknown server state: $other", cursor.history))

/** One server a host is holding. `id` is the server's name in the language pack, e.g. `scala`. */
final case class HostedServer(id: String, pid: Long, state: ServerState, identity: LaunchIdentity)

/** What a host publishes about itself. It holds no secret: the token lives in its own file. */
final case class HostRecord(
    pid: Long,
    root: String,
    protocolVersion: Int,
    startedAt: Instant,
    servers: List[HostedServer]
)

object HostedServer:
  given Codec[HostedServer] = deriveCodec

object HostRecord:
  given Codec[HostRecord] = deriveCodec
