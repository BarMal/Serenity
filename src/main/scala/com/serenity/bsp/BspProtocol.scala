package com.serenity.bsp

import java.net.URI
import java.nio.file.Path

import scala.util.Try

import cats.syntax.all.*
import io.circe.{ACursor, DecodingFailure, Json}

/** A build target's identifier, compared as the server wrote it. */
final case class BuildTargetId(uri: String)

final case class ServerCapabilities(compile: Boolean, test: Boolean, run: Boolean, debug: Boolean, canReload: Boolean)

final case class InitializeResult(
    displayName: String,
    version: String,
    bspVersion: String,
    capabilities: ServerCapabilities
)

final case class BspBuildTarget(
    id: BuildTargetId,
    displayName: Option[String],
    tags: List[String],
    canCompile: Boolean,
    canTest: Boolean,
    canRun: Boolean,
    canDebug: Boolean
)

/** Zero-based lines and characters, end exclusive, as BSP and LSP write them. */
final case class DiagnosticRange(startLine: Int, startCharacter: Int, endLine: Int, endCharacter: Int)

final case class BspDiagnostic(
    range: DiagnosticRange,
    severity: Option[Int],
    message: String,
    code: Option[String],
    source: Option[String]
)

/** `reset` replaces what the server said before about this document and target; otherwise these are added to it. */
final case class PublishDiagnostics(
    document: Path,
    target: BuildTargetId,
    originId: Option[String],
    diagnostics: List[BspDiagnostic],
    reset: Boolean
)

final case class RpcError(code: Int, message: String)

enum CompileOutcome:
  case Ok
  case Failed(message: String)
  case Cancelled
  case ServerError(code: Int, message: String)

enum BspNotification:
  case TaskStarted(taskId: String, message: Option[String], originId: Option[String])
  case TaskProgressed(taskId: String, message: Option[String], progress: Option[Long], total: Option[Long])
  case TaskFinished(taskId: String, message: Option[String], status: Int, originId: Option[String])
  case Diagnostics(params: PublishDiagnostics)
  case Log(level: Int, message: String, originId: Option[String])
  case TargetsChanged(targets: List[BuildTargetId])
  case Other(method: String)

enum BspInbound:
  case Response(id: Long, outcome: Either[RpcError, Json])
  case Notification(notification: BspNotification)
  case Request(id: Json, method: String)

/** Reading and writing the subset of the Build Server Protocol that Serenity uses
  * (https://build-server-protocol.github.io/docs/specification). Framing is `LspFramer`'s.
  */
object BspProtocol:

  /** What sbt answers a failed compile with: an error response rather than `statusCode: 2`, as recorded from sbt 1.10.7
    * in #2522.
    */
  private val InternalError = -32603

  def request(id: Long, method: String, params: Json): Json =
    Json.obj(
      "jsonrpc" -> Json.fromString("2.0"),
      "id"      -> Json.fromLong(id),
      "method"  -> Json.fromString(method),
      "params"  -> params
    )

  def notification(method: String, params: Json): Json =
    Json.obj("jsonrpc" -> Json.fromString("2.0"), "method" -> Json.fromString(method), "params" -> params)

  def initializeParams(root: URI, displayName: String, version: String, languageIds: List[String]): Json =
    Json.obj(
      "displayName"  -> Json.fromString(displayName),
      "version"      -> Json.fromString(version),
      "bspVersion"   -> Json.fromString("2.1.0"),
      "rootUri"      -> Json.fromString(root.toString),
      "capabilities" -> Json.obj("languageIds" -> Json.fromValues(languageIds.map(Json.fromString)))
    )

  def compileParams(targets: List[BuildTargetId], originId: String): Json =
    Json.obj("targets" -> targetList(targets), "originId" -> Json.fromString(originId))

  def runParams(target: BuildTargetId, originId: String): Json =
    Json.obj("target" -> targetId(target), "originId" -> Json.fromString(originId))

  def inbound(message: Json): Either[DecodingFailure, BspInbound] =
    val cursor = message.hcursor
    (cursor.get[Option[String]]("method"), cursor.downField("id").focus) match
      case (Right(Some(method)), Some(id)) => Right(BspInbound.Request(id, method))
      case (Right(Some(method)), None) =>
        notificationOf(method, cursor.downField("params")).map(BspInbound.Notification(_))
      case (Right(None), Some(_)) => responseOf(message)
      case (Left(failure), _)     => Left(failure)
      case (Right(None), None) =>
        Left(DecodingFailure("neither a request, a notification nor a response", cursor.history))

  def initializeResult(result: Json): Either[DecodingFailure, InitializeResult] =
    val c            = result.hcursor
    val capabilities = c.downField("capabilities")
    for
      name    <- c.get[String]("displayName")
      version <- c.get[String]("version")
      bsp     <- c.get[String]("bspVersion")
      reload  <- capabilities.getOrElse[Boolean]("canReload")(false)
    yield InitializeResult(
      name,
      version,
      bsp,
      ServerCapabilities(
        compile = provides(capabilities, "compileProvider"),
        test = provides(capabilities, "testProvider"),
        run = provides(capabilities, "runProvider"),
        debug = provides(capabilities, "debugProvider"),
        canReload = reload
      )
    )

  def buildTargets(result: Json): Either[DecodingFailure, List[BspBuildTarget]] =
    result.hcursor.downField("targets").as[List[Json]].flatMap(_.traverse(buildTarget))

  def compileOutcome(response: Either[RpcError, Json]): CompileOutcome =
    response match
      case Left(RpcError(InternalError, message)) if message.contains("Compilation failed") =>
        CompileOutcome.Failed(message)
      case Left(RpcError(code, message)) => CompileOutcome.ServerError(code, message)
      case Right(result) =>
        result.hcursor.get[Int]("statusCode").toOption match
          case Some(1) => CompileOutcome.Ok
          case Some(3) => CompileOutcome.Cancelled
          case _       => CompileOutcome.Failed("compile reported errors")

  /** The file a `file:` URI names, whether the server wrote `file:/` or `file:///`; sbt writes targets one way and
    * documents the other. Anything else, such as a file inside a jar, names no file.
    */
  def documentPath(uri: String): Option[Path] =
    Try(URI.create(uri)).toOption
      .filter(parsed => parsed.getScheme == "file" && parsed.getPath != null)
      .map(parsed => Path.of(parsed.getPath))

  private def provides(capabilities: ACursor, name: String): Boolean =
    capabilities.downField(name).focus.exists(json => !json.isNull && json.asBoolean.forall(identity))

  private def targetId(target: BuildTargetId): Json = Json.obj("uri" -> Json.fromString(target.uri))

  private def targetList(targets: List[BuildTargetId]): Json = Json.fromValues(targets.map(targetId))

  private def buildTarget(json: Json): Either[DecodingFailure, BspBuildTarget] =
    val c   = json.hcursor
    val can = c.downField("capabilities")
    for
      id      <- c.downField("id").get[String]("uri")
      name    <- c.get[Option[String]]("displayName")
      tags    <- c.getOrElse[List[String]]("tags")(Nil)
      compile <- can.getOrElse[Boolean]("canCompile")(false)
      test    <- can.getOrElse[Boolean]("canTest")(false)
      run     <- can.getOrElse[Boolean]("canRun")(false)
      debug   <- can.getOrElse[Boolean]("canDebug")(false)
    yield BspBuildTarget(BuildTargetId(id), name, tags, compile, test, run, debug)

  private def responseOf(message: Json): Either[DecodingFailure, BspInbound] =
    val c = message.hcursor
    c.get[Long]("id").flatMap { id =>
      c.downField("error").focus match
        case Some(error) =>
          (error.hcursor.get[Int]("code"), error.hcursor.get[String]("message")) match
            case (Right(code), Right(text)) => Right(BspInbound.Response(id, Left(RpcError(code, text))))
            case (Left(failure), _)         => Left(failure)
            case (_, Left(failure))         => Left(failure)
        case None => Right(BspInbound.Response(id, Right(c.downField("result").focus.getOrElse(Json.Null))))
    }

  private def notificationOf(method: String, params: ACursor): Either[DecodingFailure, BspNotification] =
    method match
      case "build/taskStart" =>
        for
          id      <- params.downField("taskId").get[String]("id")
          message <- params.get[Option[String]]("message")
          origin  <- params.get[Option[String]]("originId")
        yield BspNotification.TaskStarted(id, message, origin)
      case "build/taskProgress" =>
        for
          id       <- params.downField("taskId").get[String]("id")
          message  <- params.get[Option[String]]("message")
          progress <- params.get[Option[Long]]("progress")
          total    <- params.get[Option[Long]]("total")
        yield BspNotification.TaskProgressed(id, message, progress, total)
      case "build/taskFinish" =>
        for
          id      <- params.downField("taskId").get[String]("id")
          message <- params.get[Option[String]]("message")
          status  <- params.get[Int]("status")
          origin  <- params.get[Option[String]]("originId")
        yield BspNotification.TaskFinished(id, message, status, origin)
      case "build/publishDiagnostics" => publishDiagnostics(params).map(BspNotification.Diagnostics(_))
      case "build/logMessage" | "build/showMessage" =>
        for
          level   <- params.get[Int]("type")
          message <- params.get[String]("message")
          origin  <- params.get[Option[String]]("originId")
        yield BspNotification.Log(level, message, origin)
      case "buildTarget/didChange" =>
        params
          .downField("changes")
          .as[List[Json]]
          .flatMap(_.traverse(_.hcursor.downField("target").get[String]("uri").map(BuildTargetId(_))))
          .map(BspNotification.TargetsChanged(_))
      case other => Right(BspNotification.Other(other))

  private def publishDiagnostics(params: ACursor): Either[DecodingFailure, PublishDiagnostics] =
    for
      uri         <- params.downField("textDocument").get[String]("uri")
      document    <- documentPath(uri).toRight(DecodingFailure(s"not a file URI: $uri", params.history))
      target      <- params.downField("buildTarget").get[String]("uri")
      origin      <- params.get[Option[String]]("originId")
      reset       <- params.get[Boolean]("reset")
      diagnostics <- params.downField("diagnostics").as[List[Json]].flatMap(_.traverse(diagnostic))
    yield PublishDiagnostics(document, BuildTargetId(target), origin, diagnostics, reset)

  private def diagnostic(json: Json): Either[DecodingFailure, BspDiagnostic] =
    val c     = json.hcursor
    val range = c.downField("range")
    for
      startLine <- range.downField("start").get[Int]("line")
      startChar <- range.downField("start").get[Int]("character")
      endLine   <- range.downField("end").get[Int]("line")
      endChar   <- range.downField("end").get[Int]("character")
      severity  <- c.get[Option[Int]]("severity")
      message   <- c.get[String]("message")
      code      <- c.downField("code").focus.fold(Right(None))(codeText)
      source    <- c.get[Option[String]]("source")
    yield BspDiagnostic(DiagnosticRange(startLine, startChar, endLine, endChar), severity, message, code, source)

  /** A diagnostic's code may be a string or a number. */
  private def codeText(json: Json): Either[DecodingFailure, Option[String]] =
    Right(json.asString.orElse(json.asNumber.map(_.toString)))
