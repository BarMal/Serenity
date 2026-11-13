package com.serenity.app

import com.serenity.app.RendererChoice.{Candidate, Outcome}
import io.circe.syntax.*
import io.circe.{Decoder, HCursor, Json}

/** What a renderer trial child prints, and how the coordinator reads it (#2618).
  *
  * A child prints one `RENDERER_TRIAL {json}` line: the mode it was asked for, the backend the window really ran on,
  * the render API the layer ended with, and the outcome of the workload. The coordinator trusts nothing else in the
  * child's output. A child that printed no line, an unreadable one or several, or whose window ran on another backend
  * than the mode needs (a layer that fell back to the terminal, say), is a failed trial, never a measurement of that mode.
  */
object RendererTrial:

  val Marker: String = "RENDERER_TRIAL"

  final case class Report(requested: Candidate, backend: RenderBackend, renderApi: Option[String], outcome: Outcome)

  def line(report: Report): String =
    val json = Json.obj(
      "requested" -> report.requested.label.asJson,
      "backend"   -> report.backend.configKey.asJson,
      "renderApi" -> report.renderApi.asJson,
      "outcome"   -> RendererBenchmarkResult.outcomeJson(report.outcome)
    )
    s"$Marker ${json.noSpaces}"

  private given Decoder[Candidate] =
    Decoder.decodeString.emap(label => Candidate.values.find(_.label == label).toRight(s"not a candidate: $label"))

  private given Decoder[RenderBackend] =
    Decoder.decodeString.emap(key => RenderBackend.values.find(_.configKey == key).toRight(s"not a backend: $key"))

  private given Decoder[Report] = (cursor: HCursor) =>
    for
      requested <- cursor.get[Candidate]("requested")
      backend   <- cursor.get[RenderBackend]("backend")
      renderApi <- cursor.get[Option[String]]("renderApi")
      outcome   <- cursor.downField("outcome").as[Outcome](using RendererBenchmarkResult.outcomeDecoder)
    yield Report(requested, backend, renderApi, outcome)

  def read(text: String): Option[Report] =
    Option
      .when(text.startsWith(s"$Marker "))(text.drop(Marker.length + 1))
      .flatMap(json => io.circe.parser.parse(json).toOption)
      .flatMap(_.as[Report].toOption)

  private def backendFor(candidate: Candidate): RenderBackend = candidate match
    case Candidate.LayerImage | Candidate.LayerPicture => RenderBackend.Skia
    case Candidate.SkiaRaster                          => RenderBackend.SkiaRaster

  /** The outcome of the trial a child was asked to run as `expected`, from everything it printed. */
  def outcomeOf(output: String, expected: Candidate): Outcome =
    output.linesIterator.filter(_.startsWith(Marker)).toList match
      case Nil => Outcome.Failed(s"the child printed no $Marker line")
      case line :: Nil =>
        read(line)
          .fold(Outcome.Failed(s"the child's $Marker line is unreadable: ${line.take(200)}"))(verdict(_, expected))
      case _ => Outcome.Failed(s"the child printed more than one $Marker line")

  private def verdict(report: Report, expected: Candidate): Outcome =
    if report.requested != expected then
      Outcome.Failed(
        s"asked for ${expected.label} but the child reports ${report.requested.label} (backend ${report.backend.configKey})"
      )
    else if report.backend != backendFor(expected) then
      Outcome.Failed(s"asked for ${expected.label} but the window ran on ${report.backend.configKey}")
    else report.outcome
