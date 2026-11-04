package com.serenity.perf

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import cats.effect.unsafe.implicits.global

/** The checked-in frame counts and the one place that rewrites them: `sbt writeFrameCountBaseline`. The ratchet spec
  * only ever reads the file, so changing a count is an act the author of the change performs and commits.
  */
object FrameCountBaseline:

  /** Where the spec finds the baseline on the test classpath. */
  val Resource = "/perf/frame-counts.tsv"

  val SourceFile: Path = Paths.get("src", "test", "resources", "perf", "frame-counts.tsv")

  val RegenerateCommand = "sbt writeFrameCountBaseline"

  /** The scale the baseline is measured at: the laptop's 2x, where a persisting frame is most expensive. */
  val DeviceScale = 2.0

  private val Header =
    s"""# Frame counts pinned by FrameCountRatchetSpec: scenario.counter, value.
       |# Any change, up or down, fails the spec. If the change is intended, run `$RegenerateCommand`
       |# and commit this file with a line in the PR saying why the counts moved.
       |""".stripMargin

  /** A count that differs from the baseline; `None` on one side is a count that exists only on the other. */
  final case class Change(key: String, baseline: Option[Long], now: Option[Long])

  def parse(lines: Seq[String]): Either[String, Vector[(String, Long)]] =
    lines
      .map(_.trim)
      .filterNot(line => line.isEmpty || line.startsWith("#"))
      .toVector
      .foldLeft(
        Right(Vector.empty[(String, Long)]): Either[String, Vector[(String, Long)]]
      ) { (parsed, line) =>
        parsed.flatMap { entries =>
          line.split('\t') match
            case Array(key, value) =>
              value.toLongOption
                .toRight(s"not a number in the frame-count baseline: '$line'")
                .map(n => entries :+ (key -> n))
            case _ => Left(s"expected 'scenario.counter<TAB>value' in the frame-count baseline, got '$line'")
        }
      }

  def render(entries: Seq[(String, Long)]): String =
    Header + entries.map((key, value) => s"$key\t$value").mkString("", "\n", "\n")

  /** Every key whose count is not the same on both sides, in baseline order, then keys the baseline lacks. */
  def changes(baseline: Seq[(String, Long)], now: Seq[(String, Long)]): List[Change] =
    val before = baseline.toMap
    val after  = now.toMap
    val keys   = (baseline.map(_._1) ++ now.map(_._1)).distinct
    keys.toList.map(key => Change(key, before.get(key), after.get(key))).filter(change => change.baseline != change.now)

  /** The changes as the table the failure prints: scenario, counter, baseline, now. */
  def table(changes: Seq[Change]): String =
    def cell(count: Option[Long]): String = count.fold("(absent)")(_.toString)
    val rows = changes.map { change =>
      val (scenario, counter) = change.key.span(_ != '.')
      List(scenario, counter.stripPrefix("."), cell(change.baseline), cell(change.now))
    }
    val all    = List("scenario", "counter", "baseline", "now") :: rows.toList
    val widths = all.transpose.map(_.map(_.length).max)
    all
      .map(row => row.zip(widths).map((text, width) => text.padTo(width, ' ')).mkString("  ").trim)
      .mkString("\n")

  def load(): Either[String, Vector[(String, Long)]] =
    Option(getClass.getResourceAsStream(Resource)).toRight(s"$Resource is not on the test classpath").flatMap {
      stream =>
        val text =
          try new String(stream.readAllBytes(), StandardCharsets.UTF_8)
          finally stream.close()
        parse(text.linesIterator.toList)
    }

  /** `sbt writeFrameCountBaseline`: measures every scenario and overwrites [[SourceFile]]. */
  def main(args: Array[String]): Unit =
    val measured = FrameCountScenarios.entries(DeviceScale)
    Files.createDirectories(SourceFile.getParent)
    val _ = Files.writeString(SourceFile, render(measured), StandardCharsets.UTF_8)
    println(s"frame-count baseline written: ${measured.size} counts at $SourceFile")

end FrameCountBaseline
