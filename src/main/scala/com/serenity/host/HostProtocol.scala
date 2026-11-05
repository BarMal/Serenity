package com.serenity.host

import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

/** What the one attached Serenity may ask of a host. */
enum HostRequest:
  case Hello(token: String, protocolVersion: Int, clientPid: Long)
  case Status
  case Detach
  case Stop
  case RestartServer(serverId: String)

enum Refusal:
  case BadToken
  case UnsupportedProtocol(supported: Int)
  case UnknownServer(serverId: String)

enum HostResponse:
  case Welcome(record: HostRecord)
  case Busy(heldByPid: Long)
  case Refused(reason: Refusal)
  case Ok
  case Report(record: HostRecord)

/** Each message is one line of JSON. A field the reader does not know is ignored, so a newer client can add one; a
  * message of an unknown type is an error, and the host closes that connection.
  */
object HostProtocol:

  def encode(request: HostRequest): String   = request.asJson.noSpaces
  def encode(response: HostResponse): String = response.asJson.noSpaces

  def decodeRequest(line: String): Either[io.circe.Error, HostRequest] =
    io.circe.parser.decode[HostRequest](line)

  def decodeResponse(line: String): Either[io.circe.Error, HostResponse] =
    io.circe.parser.decode[HostResponse](line)

  private def unknown(what: String, value: String, cursor: io.circe.HCursor): DecodingFailure =
    DecodingFailure(s"unknown $what: $value", cursor.history)

  given Encoder[HostRequest] = Encoder.instance:
    case HostRequest.Hello(token, version, pid) =>
      Json.obj(
        "type"            -> "hello".asJson,
        "token"           -> token.asJson,
        "protocolVersion" -> version.asJson,
        "clientPid"       -> pid.asJson
      )
    case HostRequest.Status                => Json.obj("type" -> "status".asJson)
    case HostRequest.Detach                => Json.obj("type" -> "detach".asJson)
    case HostRequest.Stop                  => Json.obj("type" -> "stop".asJson)
    case HostRequest.RestartServer(server) => Json.obj("type" -> "restartServer".asJson, "serverId" -> server.asJson)

  given Decoder[HostRequest] = Decoder.instance: cursor =>
    cursor
      .get[String]("type")
      .flatMap:
        case "hello" =>
          for
            token   <- cursor.get[String]("token")
            version <- cursor.get[Int]("protocolVersion")
            pid     <- cursor.get[Long]("clientPid")
          yield HostRequest.Hello(token, version, pid)
        case "status"        => Right(HostRequest.Status)
        case "detach"        => Right(HostRequest.Detach)
        case "stop"          => Right(HostRequest.Stop)
        case "restartServer" => cursor.get[String]("serverId").map(HostRequest.RestartServer(_))
        case other           => Left(unknown("request", other, cursor))

  private given Encoder[Refusal] = Encoder.instance:
    case Refusal.BadToken                 => Json.obj("kind" -> "badToken".asJson)
    case Refusal.UnsupportedProtocol(top) => Json.obj("kind" -> "unsupportedProtocol".asJson, "supported" -> top.asJson)
    case Refusal.UnknownServer(server)    => Json.obj("kind" -> "unknownServer".asJson, "serverId" -> server.asJson)

  private given Decoder[Refusal] = Decoder.instance: cursor =>
    cursor
      .get[String]("kind")
      .flatMap:
        case "badToken"            => Right(Refusal.BadToken)
        case "unsupportedProtocol" => cursor.get[Int]("supported").map(Refusal.UnsupportedProtocol(_))
        case "unknownServer"       => cursor.get[String]("serverId").map(Refusal.UnknownServer(_))
        case other                 => Left(unknown("refusal", other, cursor))

  given Encoder[HostResponse] = Encoder.instance:
    case HostResponse.Welcome(record) => Json.obj("type" -> "welcome".asJson, "record" -> record.asJson)
    case HostResponse.Busy(pid)       => Json.obj("type" -> "busy".asJson, "heldByPid" -> pid.asJson)
    case HostResponse.Refused(reason) => Json.obj("type" -> "refused".asJson, "reason" -> reason.asJson)
    case HostResponse.Ok              => Json.obj("type" -> "ok".asJson)
    case HostResponse.Report(record)  => Json.obj("type" -> "report".asJson, "record" -> record.asJson)

  given Decoder[HostResponse] = Decoder.instance: cursor =>
    cursor
      .get[String]("type")
      .flatMap:
        case "welcome" => cursor.get[HostRecord]("record").map(HostResponse.Welcome(_))
        case "busy"    => cursor.get[Long]("heldByPid").map(HostResponse.Busy(_))
        case "refused" => cursor.get[Refusal]("reason").map(HostResponse.Refused(_))
        case "ok"      => Right(HostResponse.Ok)
        case "report"  => cursor.get[HostRecord]("record").map(HostResponse.Report(_))
        case other     => Left(unknown("response", other, cursor))
