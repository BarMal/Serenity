package com.serenity.perf

/** The numbers and the text of the windowed renderer comparison, apart from the processes that produce them, so how a
  * table is made can be checked without opening a window.
  *
  * Each renderer is measured in a child JVM of its own, because the backend is chosen once at startup. A child prints
  * its environment and one line per frame; the parent pools the frames of a renderer across rounds and reports
  * percentiles of the pool.
  */
private[perf] object RendererBenchmarkReport:

  /** A renderer to compare, and the launch flags that select it exactly as a user would. */
  enum Renderer(val name: String, val flags: List[String]):
    case Java2D     extends Renderer("java2d", List("--renderer=java2d"))
    case Skia       extends Renderer("skia", List("--renderer=skia"))
    case SkiaRaster extends Renderer("skia-raster", List("--renderer=skia", "--wayland-render=skia-raster"))

  object Renderer:

    def parseAll(csv: String): Either[String, List[Renderer]] =
      val names   = csv.split(',').toList.map(_.trim).filter(_.nonEmpty)
      val parsed  = names.map(name => Renderer.values.find(_.name == name).toRight(name))
      val unknown = parsed.collect { case Left(name) => name }
      if names.isEmpty then Left("no renderer named; expected a list of java2d, skia, skia-raster")
      else if unknown.nonEmpty then
        Left(s"unknown renderer ${unknown.mkString(", ")}; expected java2d, skia or skia-raster")
      else Right(parsed.collect { case Right(renderer) => renderer }.distinct)

  /** One frame of one scenario: how long it took and what the JVM allocated while it ran. */
  final case class Sample(scenario: String, nanos: Long, allocatedBytes: Long)

  /** What a child reports about itself: the setting it ran with, and the frames it timed. */
  final case class ChildReport(environment: Map[String, String], samples: List[Sample]):
    def backend: String = environment.getOrElse("backend", "unknown")
    def api: String     = environment.getOrElse("api", "unknown")

  private val EnvironmentTag = "ENV"
  private val SampleTag      = "SAMPLE"

  def environmentLine(environment: Map[String, String]): String =
    (EnvironmentTag :: environment.toList.sortBy(_._1).map((key, value) => s"$key=${value.replace('\t', ' ')}"))
      .mkString("\t")

  def sampleLine(sample: Sample): String =
    List(SampleTag, sample.scenario, sample.nanos.toString, sample.allocatedBytes.toString).mkString("\t")

  /** Reads a child's output; any line that is neither tag is the child's own chatter and is skipped. */
  def parseChild(output: List[String]): ChildReport =
    val environment = output
      .filter(_.startsWith(EnvironmentTag + "\t"))
      .flatMap(_.split('\t').toList.drop(1))
      .flatMap { pair =>
        val at = pair.indexOf('=')
        Option.when(at > 0)(pair.substring(0, at) -> pair.substring(at + 1))
      }
      .toMap
    val samples = output.filter(_.startsWith(SampleTag + "\t")).flatMap { line =>
      line.split('\t').toList match
        case _ :: scenario :: nanos :: allocated :: Nil =>
          for
            n <- nanos.toLongOption
            a <- allocated.toLongOption
          yield Sample(scenario, n, a)
        case _ => None
    }
    ChildReport(environment, samples)

  /** A child that was asked for one renderer counts for it only if the editor's own startup decision chose it. A Skia
    * request that fell back to Java2D would otherwise be reported as a Skia result.
    */
  def checked(renderer: Renderer, report: ChildReport): Either[String, ChildReport] =
    Either.cond(
      report.backend == renderer.name,
      report,
      s"startup chose ${report.backend}: ${report.environment.getOrElse("reason", "no reason given")}"
    )

  /** Nearest-rank percentile of already-sorted values, as `BenchmarkRunner` takes it. */
  def percentile(sorted: IndexedSeq[Double], fraction: Double): Double =
    if sorted.isEmpty then 0.0
    else sorted(math.ceil(fraction.max(0.0).min(1.0) * sorted.length).toInt.max(1).min(sorted.length) - 1)

  /** One line of the table: a renderer and a scenario, or why the renderer could not be measured. */
  final case class Row(
      renderer: String,
      api: String,
      scenario: String,
      frames: Int,
      p50Ms: Double,
      p95Ms: Double,
      p99Ms: Double,
      allocatedKiB: Double
  )

  final case class Unavailable(renderer: String, reason: String)

  def rows(renderer: Renderer, reports: List[ChildReport]): List[Row] =
    val api     = reports.headOption.map(_.api).getOrElse("unknown")
    val samples = reports.flatMap(_.samples)
    samples.map(_.scenario).distinct.map { scenario =>
      val frames = samples.filter(_.scenario == scenario)
      val millis = frames.map(_.nanos / 1_000_000.0).sorted.toIndexedSeq
      Row(
        renderer.name,
        api,
        scenario,
        frames.length,
        percentile(millis, 0.50),
        percentile(millis, 0.95),
        percentile(millis, 0.99),
        frames.map(_.allocatedBytes).sum.toDouble / frames.length / 1024.0
      )
    }

  private val Header =
    List("renderer", "render api", "frame", "frames", "p50 ms", "p95 ms", "p99 ms", "alloc KiB/frame")

  private def cells(row: Row): List[String] =
    List(
      row.renderer,
      row.api,
      row.scenario,
      row.frames.toString,
      fixed(row.p50Ms, 2),
      fixed(row.p95Ms, 2),
      fixed(row.p99Ms, 2),
      fixed(row.allocatedKiB, 0)
    )

  private def fixed(value: Double, digits: Int): String =
    BigDecimal(value).setScale(digits, BigDecimal.RoundingMode.HALF_UP).toString

  /** The comparison as one table, in the order the renderers were asked for. */
  def table(rows: List[Row], unavailable: List[Unavailable]): List[String] =
    val body   = rows.map(cells)
    val widths = Header.indices.map(i => (Header(i) :: body.map(_(i))).map(_.length).max)
    def line(values: List[String]): String =
      values.zipWithIndex
        .map((value, i) => if i < 3 then value.padTo(widths(i), ' ') else value.reverse.padTo(widths(i), ' ').reverse)
        .mkString("  ")
        .trim
    val rule = widths.map("-" * _).mkString("  ")
    line(Header) :: rule :: body.map(line) ++ unavailable.map(u => s"${u.renderer}: unavailable here, ${u.reason}")
