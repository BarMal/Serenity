package com.serenity.app

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.time.Instant

import cats.effect.IO
import com.serenity.app.RendererChoice.{Candidate, CandidateRuns, Outcome, Workload}
import io.circe.{Decoder, HCursor, Json}
import io.circe.syntax.*

/** What `serenity --optimize-renderer` measured on this machine, kept beside the config so the next run can say whether
  * it still applies (#2618).
  *
  * A result is only as good as the setup it was measured on: it is read as stale as soon as any part of the [[Key]]
  * differs, and a stale result is shown, never applied. A file that cannot be read back is the same as no file.
  */
object RendererBenchmarkResult:

  /** Everything that can change which renderer is fastest. */
  final case class Key(
      glRenderer: String,
      os: String,
      toolkit: String,
      display: String,
      jvm: String,
      skiko: String,
      powerState: String
  ):

    def differencesFrom(other: Key): List[String] =
      List(
        "GL renderer" -> (glRenderer != other.glRenderer),
        "OS"          -> (os != other.os),
        "toolkit"     -> (toolkit != other.toolkit),
        "display"     -> (display != other.display),
        "JVM"         -> (jvm != other.jvm),
        "Skiko"       -> (skiko != other.skiko),
        "power state" -> (powerState != other.powerState)
      ).collect { case (name, true) => name }

  final case class Stored(key: Key, measuredAt: Instant, results: List[CandidateRuns])

  enum Standing:
    case Absent
    case Applies(stored: Stored)
    case Stale(stored: Stored, changed: List[String])

  val fileName: String = "render-benchmark.json"

  def defaultPath: Path = Paths.get(System.getProperty("user.home"), ".serenity", fileName)

  def standing(current: Key, stored: Option[Stored]): Standing =
    stored.fold(Standing.Absent) { result =>
      result.key.differencesFrom(current) match
        case Nil     => Standing.Applies(result)
        case changed => Standing.Stale(result, changed)
    }

  private def keyJson(key: Key): Json = Json.obj(
    "glRenderer" -> key.glRenderer.asJson,
    "os"         -> key.os.asJson,
    "toolkit"    -> key.toolkit.asJson,
    "display"    -> key.display.asJson,
    "jvm"        -> key.jvm.asJson,
    "skiko"      -> key.skiko.asJson,
    "powerState" -> key.powerState.asJson
  )

  private def outcomeJson(outcome: Outcome): Json = outcome match
    case Outcome.Measured(workload) =>
      Json.obj("typingP50Ms" -> workload.typingP50Ms.asJson, "fullWindowMs" -> workload.fullWindowMs.asJson)
    case Outcome.Failed(reason) =>
      Json.obj("failed" -> reason.asJson)

  private def resultsJson(results: List[CandidateRuns]): Json =
    Json.arr(results.map { runs =>
      Json.obj("candidate" -> runs.candidate.label.asJson, "runs" -> Json.arr(runs.runs.map(outcomeJson)*))
    }*)

  def encode(stored: Stored): String =
    Json
      .obj(
        "key"        -> keyJson(stored.key),
        "measuredAt" -> stored.measuredAt.toString.asJson,
        "results"    -> resultsJson(stored.results)
      )
      .spaces2

  private given Decoder[Key] = (cursor: HCursor) =>
    for
      glRenderer <- cursor.get[String]("glRenderer")
      os         <- cursor.get[String]("os")
      toolkit    <- cursor.get[String]("toolkit")
      display    <- cursor.get[String]("display")
      jvm        <- cursor.get[String]("jvm")
      skiko      <- cursor.get[String]("skiko")
      powerState <- cursor.get[String]("powerState")
    yield Key(glRenderer, os, toolkit, display, jvm, skiko, powerState)

  private given Decoder[Instant] = Decoder.decodeString.emap(text =>
    Right(text).flatMap(raw => scala.util.Try(Instant.parse(raw)).toEither.left.map(_ => s"not a time: $raw"))
  )

  private given Decoder[Candidate] =
    Decoder.decodeString.emap(label => Candidate.values.find(_.label == label).toRight(s"not a candidate: $label"))

  private given Decoder[Outcome] = (cursor: HCursor) =>
    cursor.downField("failed").as[Option[String]].flatMap {
      case Some(reason) => Right(Outcome.Failed(reason))
      case None =>
        for
          typing <- cursor.get[Double]("typingP50Ms")
          rows   <- cursor.get[Map[String, Double]]("fullWindowMs")
        yield Outcome.Measured(Workload(typing, rows))
    }

  private given Decoder[CandidateRuns] = (cursor: HCursor) =>
    for
      candidate <- cursor.get[Candidate]("candidate")
      runs      <- cursor.get[List[Outcome]]("runs")
    yield CandidateRuns(candidate, runs)

  private given Decoder[Stored] = (cursor: HCursor) =>
    for
      key        <- cursor.get[Key]("key")
      measuredAt <- cursor.get[Instant]("measuredAt")
      results    <- cursor.get[List[CandidateRuns]]("results")
    yield Stored(key, measuredAt, results)

  def decode(text: String): Option[Stored] =
    io.circe.parser.parse(text).toOption.flatMap(_.as[Stored].toOption)

  def load(path: Path): IO[Option[Stored]] =
    IO.blocking(new String(Files.readAllBytes(path), StandardCharsets.UTF_8))
      .map(decode)
      .handleError(_ => None)

  /** Written to a sibling file and moved into place, so a reader never sees half a result. */
  def save(path: Path, stored: Stored): IO[Unit] =
    IO.blocking {
      val directory = Option(path.toAbsolutePath.getParent).getOrElse(path.toAbsolutePath)
      val _         = Files.createDirectories(directory)
      val temporary = Files.createTempFile(directory, fileName, ".tmp")
      val _         = Files.write(temporary, encode(stored).getBytes(StandardCharsets.UTF_8))
      val _         = Files.move(temporary, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
    }
