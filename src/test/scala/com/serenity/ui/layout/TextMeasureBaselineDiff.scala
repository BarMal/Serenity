package com.serenity.ui.layout

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import com.serenity.ui.layout.TextMeasureBaseline.Entry
import com.serenity.ui.layout.TextMeasureClassification.Reason
import com.serenity.ui.layout.TextMeasureParity.Compared
import com.serenity.ui.skia.{SkiaTextMeasurer, TestBundledTypefaces}

/** Every row of `text-measure/baseline.tsv` that moved when layout went from AWT to the Skia measurer (#2660 item 2,
  * step D2), each with the class of [[TextMeasureClassification]] it falls in: a caret by its own class, a line height
  * by the rounding Skia's line box takes, a wrap row by the carets it follows, a shaping row by what HarfBuzz does that
  * AWT's glyph vector did not. Run
  * `app/Test/runMain com.serenity.ui.layout.TextMeasureBaselineDiff <awt> <skia> <out>`.
  */
object TextMeasureBaselineDiff:

  final case class Row(key: String, awt: Vector[String], skia: Vector[String], reason: String, accepted: Boolean)

  private val faces    = TestBundledTypefaces.cacheWithInter()
  private val measurer = new SkiaTextMeasurer(faces)

  private val WrapKinds = Set("rows", "ends", "widths")

  private def kindOf(key: String): String = key.split('.').last.takeWhile(_ != '@')

  private def lineOf(key: String): String = key.split('.').take(2).mkString(".")

  private def same(kind: String, a: Vector[String], b: Vector[String]): Boolean =
    a.length == b.length && a.zip(b).forall { (x, y) =>
      if Set("carets", "widths").contains(kind) then
        math.abs(x.toDouble - y.toDouble) <= TextMeasureBaseline.FloatTolerance + 1e-6
      else x == y
    }

  private def caretReasons(key: String, awt: Vector[String], skia: Vector[String]): Vector[Reason] =
    val compared = awt.zip(skia).zipWithIndex.collect {
      case ((a, s), index) if math.abs(a.toDouble - s.toDouble) > TextMeasureBaseline.FloatTolerance + 1e-6 =>
        Compared(key, index, a.toDouble, s.toDouble, isFloat = true)
    }
    TextMeasureClassification.classify(compared, measurer, faces).map(_.reason)

  def rows(awt: Seq[Entry], skia: Seq[Entry]): Vector[Row] =
    val before = awt.map(entry => entry.key -> entry.values).toMap
    val after  = skia.map(entry => entry.key -> entry.values).toMap
    val keys   = (awt.map(_.key) ++ skia.map(_.key)).distinct.toVector
    val moved  = keys.filterNot(key => before.get(key).zip(after.get(key)).exists((a, s) => same(kindOf(key), a, s)))
    val caretVerdicts = moved
      .filter(kindOf(_) == "carets")
      .map { key =>
        val (a, s) = (before.getOrElse(key, Vector.empty), after.getOrElse(key, Vector.empty))
        lineOf(key) -> (if a.length != s.length then Vector(Reason.Flagged) else caretReasons(key, a, s))
      }
      .toMap
    moved.map { key =>
      val (a, s)             = (before.getOrElse(key, Vector.empty), after.getOrElse(key, Vector.empty))
      val (reason, accepted) = classify(key, a, s, caretVerdicts)
      Row(key, a, s, reason, accepted)
    }

  private def label(reason: Reason): (String, Boolean) = (reason.label, reason.accepted)

  private def classify(
    key: String,
    awt: Vector[String],
    skia: Vector[String],
    caretVerdicts: Map[String, Vector[Reason]]
  ): (String, Boolean) =
    val line = key.split('.').lift(1).getOrElse("")
    kindOf(key) match
      case _ if line == "emoji_clusters" =>
        ("accepted: emoji sequences measured from the drawn stretch", true)
      case "cell" =>
        val heightOnly = awt.length == 3 && skia.length == 3 && awt(0) == skia(0) && awt(2) == skia(2) &&
          math.abs(awt(1).toInt - skia(1).toInt) <= 1
        if heightOnly then label(Reason.Rounding) else label(Reason.Flagged)
      case "heights" =>
        val oneOff = awt.length == skia.length &&
          awt.zip(skia).forall((a, s) => math.abs(a.toInt - s.toInt) <= 1)
        if oneOff then label(Reason.Rounding)
        else followsCarets(key, caretVerdicts, "accepted: the row count follows accepted caret changes")
      case "carets" =>
        val reasons = caretVerdicts.getOrElse(lineOf(key), Vector(Reason.Flagged))
        label(reasons.find(!_.accepted).getOrElse(reasons.groupBy(identity).maxBy(_._2.size)._1))
      case kind if WrapKinds.contains(kind) =>
        followsCarets(key, caretVerdicts, "accepted: wrap points follow accepted caret changes")
      case "glyphs" | "clusters" if line == "ltr_rtl" =>
        ("accepted: HarfBuzz lists a right-to-left run's glyphs in visual order, AWT's vector in logical", true)
      case "glyphs" | "clusters" if awt.headOption.zip(skia.headOption).isEmpty =>
        label(Reason.Flagged)
      case "glyphs" | "clusters" =>
        ("accepted: HarfBuzz forms the font's default ligatures, AWT's glyph vector did not", true)
      case _ => label(Reason.Flagged)

  private def followsCarets(key: String, verdicts: Map[String, Vector[Reason]], text: String): (String, Boolean) =
    verdicts.get(lineOf(key)) match
      case Some(reasons) if reasons.forall(_.accepted) => (text, true)
      case Some(_)                                     => ("flagged: follows flagged caret changes", false)
      case None                                        => label(Reason.Flagged)

  def report(rows: Seq[Row]): String =
    val counts = rows
      .groupBy(row => (row.accepted, row.reason))
      .toVector
      .sortBy((key, _) => (!key._1, key._2))
      .map { case ((_, reason), items) => s"# ${items.size}\t$reason" }
    val body = rows.map(row =>
      s"${row.key}\t${if row.accepted then "accepted" else "FLAGGED"}\t${row.reason}\t" +
        s"${row.awt.mkString(",")}\t${row.skia.mkString(",")}"
    )
    (s"# ${rows.size} rows of text-measure/baseline.tsv moved from AWT to the Skia layout, by class:" +: counts)
      .mkString("", "\n", "\n") + "key\tverdict\tclass\tawt\tskia\n" + body.mkString("", "\n", "\n")

  private def read(path: Path): Vector[Entry] =
    val lines = Files.readAllLines(path, StandardCharsets.UTF_8).toArray(Array.empty[String]).toVector
    TextMeasureBaseline.parse(lines).fold(reason => sys.error(reason), identity)

  def main(args: Array[String]): Unit =
    args.toList match
      case awt :: skia :: out :: Nil =>
        val _ = Files.write(Paths.get(out), report(rows(read(Paths.get(awt)), read(Paths.get(skia)))).getBytes)
      case _ => sys.error("usage: TextMeasureBaselineDiff <awt-reference.tsv> <baseline.tsv> <out.tsv>")
