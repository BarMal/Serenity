import sbt.*

import scala.io.Source

/** Structural checks the compiler and WartRemover cannot express.
  *
  * Both checks are ratchets rather than fixed thresholds. A baseline file records every location that exceeds the
  * target today; the check fails when a file gets worse, when a new file starts out over target, or when a baseline
  * entry has been fixed but not removed. Existing debt therefore never blocks unrelated work, while the list can only
  * shrink.
  *
  * The number itself is the arbitrary part -- the ratchet is what makes it meaningful.
  */
object ArchitectureChecks {

  /** A method longer than this is doing more than one thing. Chosen to sit just above the 98.5% of methods already
    * under 40 lines, so it catches the tail without churning healthy code.
    */
  val MaxMethodLines = 80

  /** A backstop for file sprawl. Deliberately generous: the method check is the real signal. */
  val MaxFileLines = 600

  /** Layering rules the package structure implies but nothing enforces.
    *
    * Reducers are pure state transitions. Reaching into AWT or the layout engine from one is what
    * docs/coding-standards.md forbids and what issue #862 exists to undo; this stops it coming back.
    */
  val ForbiddenImports: Seq[(String, Seq[String], String)] = Seq(
    (
      "com/serenity/state/reducers",
      Seq("java.awt", "com.serenity.ui.fonts", "com.serenity.ui.layout.LayoutEngine", "com.serenity.ui.renderer"),
      "reducers must stay pure: take measured geometry as a parameter instead of reaching for AWT or the layout engine"
    ),
    (
      "com/serenity/state/models",
      Seq("java.awt.Graphics", "com.serenity.ui.renderer"),
      "state models describe data, not painting"
    )
  )

  final case class Violation(path: String, detail: String, measured: Int) {
    def key: String = s"$path\t$detail"
    def render: String = s"$key\t$measured"
  }

  private val MethodStart =
    """^(\s*)((?:override|private|protected|final|inline|transparent|implicit)\s+|\[[^\]]*\]\s+)*def\s+(\w+)""".r

  private def readLines(file: File): Vector[String] = {
    val source = Source.fromFile(file, "UTF-8")
    try source.getLines().toVector
    finally source.close()
  }

  private def scalaFiles(base: File): Seq[File] =
    (base ** "*.scala").get().sortBy(_.getPath)

  private def relativise(base: File, file: File): String =
    base.toPath.relativize(file.toPath).toString.replace('\\', '/')

  /** Lines from `start` until indentation returns to `indent` or shallower, ignoring blanks and comments. */
  private def bodyLength(lines: Vector[String], start: Int, indent: Int): Int = {
    var end = lines.length
    var i = start + 1
    var found = false
    while (i < lines.length && !found) {
      val line = lines(i)
      val trimmed = line.trim
      if (trimmed.nonEmpty && !trimmed.startsWith("//")) {
        val current = line.indexWhere(!_.isWhitespace)
        if (current <= indent) { end = i; found = true }
      }
      i += 1
    }
    end - start
  }

  private def methodViolations(path: String, lines: Vector[String]): Seq[Violation] =
    lines.zipWithIndex.collect {
      case (line, index) if MethodStart.findPrefixMatchOf(line).isDefined =>
        val indent = line.indexWhere(!_.isWhitespace)
        val name = MethodStart.findPrefixMatchOf(line).map(_.group(3)).getOrElse("?")
        (index, indent, name)
    }.flatMap { case (index, indent, name) =>
      val length = bodyLength(lines, index, indent)
      if (length > MaxMethodLines) Some(Violation(path, s"method $name", length)) else None
    }

  private def importViolations(path: String, lines: Vector[String]): Seq[Violation] =
    ForbiddenImports.flatMap { case (pkg, forbidden, reason) =>
      if (!path.contains(pkg)) Nil
      else
        lines.zipWithIndex.collect {
          case (line, index)
              if line.trim.startsWith("import ") && forbidden.exists(f => line.contains(f)) =>
            Violation(path, s"forbidden import at line ${index + 1}: $reason", 1)
        }
    }

  def collect(base: File): Seq[Violation] =
    scalaFiles(base).flatMap { file =>
      val path = relativise(base, file)
      val lines = readLines(file)
      val fileViolation =
        if (lines.length > MaxFileLines) Seq(Violation(path, "file length", lines.length)) else Nil
      fileViolation ++ methodViolations(path, lines) ++ importViolations(path, lines)
    }

  def readBaseline(file: File): Map[String, Int] =
    if (!file.exists()) Map.empty
    else
      readLines(file)
        .filter(line => line.trim.nonEmpty && !line.trim.startsWith("#"))
        .flatMap { line =>
          line.split('\t') match {
            case Array(path, detail, measured) => Some(s"$path\t$detail" -> measured.trim.toInt)
            case _                             => None
          }
        }
        .toMap

  def writeBaseline(file: File, violations: Seq[Violation]): Unit = {
    val header =
      Seq(
        "# Architecture-check baseline. Generated by `sbt writeArchitectureBaseline`.",
        s"# Targets: method <= $MaxMethodLines lines, file <= $MaxFileLines lines, no forbidden imports.",
        "# This list may shrink, never grow. Removing an entry is the point.",
        "# Columns: path <TAB> detail <TAB> measured"
      )
    IO.write(file, (header ++ violations.map(_.render)).mkString("", "\n", "\n"))
  }

  /** Returns the failure report, or None when the ratchet holds. */
  def check(base: File, baselineFile: File): Option[String] = {
    val baseline = readBaseline(baselineFile)
    val current = collect(base)
    val currentByKey = current.map(v => v.key -> v.measured).toMap

    val added = current.filterNot(v => baseline.contains(v.key))
    val worsened = current.filter(v => baseline.get(v.key).exists(_ < v.measured))
    val fixed = baseline.keySet.diff(currentByKey.keySet)

    val problems =
      (if (added.isEmpty) Nil
       else
         Seq(
           s"${added.size} new violation(s) -- keep new code under target:",
           added.map(v => s"  ${v.path}  ${v.detail}  ${v.measured}").mkString("\n")
         )) ++
        (if (worsened.isEmpty) Nil
         else
           Seq(
             s"${worsened.size} existing violation(s) got worse:",
             worsened
               .map(v => s"  ${v.path}  ${v.detail}  ${baseline(v.key)} -> ${v.measured}")
               .mkString("\n")
           )) ++
        (if (fixed.isEmpty) Nil
         else
           Seq(
             s"${fixed.size} baseline entr(y/ies) no longer apply -- run `sbt writeArchitectureBaseline` to bank the win:",
             fixed.toSeq.sorted.map(key => s"  ${key.replace('\t', ' ')}").mkString("\n")
           ))

    if (problems.isEmpty) None else Some(problems.mkString("\n"))
  }
}
