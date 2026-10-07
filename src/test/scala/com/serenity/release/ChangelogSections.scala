package com.serenity.release

import java.time.LocalDate

import scala.util.Try

enum ChangelogHeading:
  case Unreleased
  case Release(version: String, date: String)

final case class ChangelogSection(heading: ChangelogHeading, body: String)

object ChangelogSections:

  private val UnreleasedHeading = "## [Unreleased]"
  private val ReleaseHeading    = """## (\d+\.\d+\.\d+(?:-[0-9A-Za-z.-]+)?) — (\d{4}-\d{2}-\d{2})""".r
  private val Fence             = "```"

  final private case class Line(number: Int, text: String)

  def parse(text: String): Either[List[String], List[ChangelogSection]] =
    val lines    = text.linesIterator.toVector
    val headings = headingLines(lines)
    val parsed   = headings.map(parseHeading)
    val valid    = parsed.collect { case Right(heading) => heading }
    val errors   = parsed.collect { case Left(error) => error } ++ structureErrors(valid)

    if errors.nonEmpty then Left(errors)
    else Right(valid.zip(bodiesOf(lines, headings)).map(ChangelogSection.apply))

  def unreleased(text: String): Either[List[String], String] =
    parse(text).flatMap(
      _.collectFirst { case ChangelogSection(ChangelogHeading.Unreleased, body) => body }
        .toRight(List(s"missing $UnreleasedHeading section"))
    )

  def forVersion(text: String, version: String): Either[List[String], String] =
    parse(text).flatMap { sections =>
      sections
        .collectFirst { case ChangelogSection(ChangelogHeading.Release(`version`, _), body) => body }
        .toRight(List(s"no section for version $version"))
        .flatMap(body => Either.cond(body.nonEmpty, body, List(s"section for version $version is empty")))
    }

  private def headingLines(lines: Vector[String]): List[Line] =
    val numbered   = lines.zipWithIndex.map((text, index) => Line(index + 1, text))
    val openBefore = numbered.scanLeft(false)((open, line) => if line.text.startsWith(Fence) then !open else open)
    numbered
      .zip(openBefore)
      .collect { case (line, false) if line.text.startsWith("## ") => line }
      .toList

  private def parseHeading(line: Line): Either[String, ChangelogHeading] =
    line.text.stripTrailing match
      case UnreleasedHeading => Right(ChangelogHeading.Unreleased)
      case ReleaseHeading(version, date) if Try(LocalDate.parse(date)).isSuccess =>
        Right(ChangelogHeading.Release(version, date))
      case _ =>
        Left(
          s"line ${line.number}: malformed heading '${line.text}'; " +
            s"expected '$UnreleasedHeading' or '## X.Y.Z — YYYY-MM-DD'"
        )

  private def structureErrors(headings: List[ChangelogHeading]): List[String] =
    val unreleasedCount = headings.count(_ == ChangelogHeading.Unreleased)
    val versions        = headings.collect { case ChangelogHeading.Release(version, _) => version }
    val duplicates = versions.diff(versions.distinct).distinct.map(version => s"duplicate section for version $version")
    val missing    = Option.when(unreleasedCount == 0)(s"missing $UnreleasedHeading section")
    val repeated   = Option.when(unreleasedCount > 1)(s"more than one $UnreleasedHeading section")
    val misplaced = Option.when(unreleasedCount == 1 && !headings.headOption.contains(ChangelogHeading.Unreleased))(
      s"$UnreleasedHeading must be the first section"
    )
    missing.toList ++ repeated ++ misplaced ++ duplicates

  private def bodiesOf(lines: Vector[String], headings: List[Line]): List[String] =
    val starts = headings.map(_.number)
    val ends   = starts.drop(1).map(next => next - 1) :+ lines.length
    starts.zip(ends).map((start, end) => lines.slice(start, end).mkString("\n").trim)
