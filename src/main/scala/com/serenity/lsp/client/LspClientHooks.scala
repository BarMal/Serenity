package com.serenity.lsp.client

import cats.effect.IO
import com.serenity.lsp.model.{LspProgress, LspTextEdit}
import io.circe.Json

enum LspMessageLevel:
  case Error, Warning, Info, Log

/** A `window/showMessage`, `window/logMessage` or `window/showMessageRequest` text. */
final case class LspServerMessage(level: LspMessageLevel, text: String)

final case class LspProgressUpdate(token: String, progress: LspProgress)

/** A `workspace/applyEdit` request: the server asks the client to change documents on its behalf. */
final case class LspApplyEditRequest(label: Option[String], edits: Map[DocumentUri, List[LspTextEdit]])

final case class LspApplyEditResult(applied: Boolean, failureReason: Option[String])

/** What a connection tells the application about the parts of a server's traffic that are not a reply to the client's
  * own requests. The connection answers the server itself; these only report, so none of them may be waited on for a
  * reply.
  */
final case class LspClientHooks(
    onMessage: LspServerMessage => IO[Unit],
    onProgress: LspProgressUpdate => IO[Unit],
    onApplyEdit: LspApplyEditRequest => IO[LspApplyEditResult]
)

object LspClientHooks:

  val ignoring: LspClientHooks = LspClientHooks(
    _ => IO.unit,
    _ => IO.unit,
    _ => IO.pure(LspApplyEditResult(applied = false, Some("The client is not ready to apply edits")))
  )

  def parseMessage(params: Json): Option[LspServerMessage] =
    val c = params.hcursor
    for
      text      <- c.downField("message").as[String].toOption
      levelCode <- c.downField("type").as[Int].toOption.orElse(Some(LevelLog))
    yield LspServerMessage(level(levelCode), text)

  def parseProgress(params: Json): Option[LspProgressUpdate] =
    val c          = params.hcursor
    val value      = c.downField("value")
    val token      = c.downField("token").focus.flatMap(raw => raw.asString.orElse(raw.asNumber.map(_.toString)))
    val message    = value.downField("message").as[String].toOption
    val percentage = value.downField("percentage").as[Int].toOption
    for
      progressToken <- token
      kind          <- value.downField("kind").as[String].toOption
      progress <- kind match
        case "begin"  => value.downField("title").as[String].toOption.map(LspProgress.Begin(_, message, percentage))
        case "report" => Some(LspProgress.Report(message, percentage))
        case "end"    => Some(LspProgress.End(message))
        case _        => None
    yield LspProgressUpdate(progressToken, progress)

  def parseApplyEdit(params: Json): Option[LspApplyEditRequest] =
    val c = params.hcursor
    c.downField("edit").focus.map { edit =>
      LspApplyEditRequest(c.downField("label").as[String].toOption, LspProtocol.parseWorkspaceEdit(edit))
    }

  def applyEditResponse(result: LspApplyEditResult): Json =
    Json.fromFields(
      List("applied" -> Json.fromBoolean(result.applied)) ++ result.failureReason.map(reason =>
        "failureReason" -> Json.fromString(reason)
      )
    )

  private val LevelLog = 4

  private def level(code: Int): LspMessageLevel =
    code match
      case 1 => LspMessageLevel.Error
      case 2 => LspMessageLevel.Warning
      case 3 => LspMessageLevel.Info
      case _ => LspMessageLevel.Log
