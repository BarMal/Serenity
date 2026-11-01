package com.serenity.perf

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.time.Instant

import scala.annotation.tailrec

/** How big and how lopsided the synthetic tree is (#2420). `filesSigma` is the spread of the log-normal weights that
  * decide how many files each directory gets; the median over the mean is exp(-sigma^2 / 2).
  */
final case class TreeShape(
    visibleFiles: Int,
    directories: Int,
    maxDepth: Int,
    filesSigma: Double,
    ignoredRoots: Int,
    ignoredFilesPerRoot: Int,
    ruleIgnoredFiles: Int
)

object TreeShape:

  /** The issue's sizes: 100,000 visible files in 10,000 directories. Basis for the rest: `maxDepth` 8 and `filesSigma`
    * 1.6 come from four real trees measured on 2026-10-30 (deepest file 6 to 9 levels; median files per directory 0.11
    * to 0.28 of the mean, and sigma 1.6 gives 0.28; see SyntheticProjectTreeSpec). The ignored-directory and
    * rule-ignored counts are assumptions: the real trees measured have none, and the walker never reads them, so they
    * only change how long the tree takes to write.
    */
  val Monorepo100k: TreeShape =
    TreeShape(
      visibleFiles = 100_000,
      directories = 10_000,
      maxDepth = 8,
      filesSigma = 1.6,
      ignoredRoots = 20,
      ignoredFilesPerRoot = 50,
      ruleIgnoredFiles = 500
    )

  /** The same shape at a size a unit test can write and walk quickly. */
  val Small: TreeShape =
    Monorepo100k.copy(
      visibleFiles = 300,
      directories = 40,
      ignoredRoots = 3,
      ignoredFilesPerRoot = 20,
      ruleIgnoredFiles = 15
    )

/** A project tree as relative paths with `/` separators; the root directory is the empty string. `files` are what the
  * walker should list besides the `.gitignore` files; the others are what it should leave out.
  */
final case class TreePlan(
    directories: Vector[String],
    files: Vector[String],
    ignoredDirectories: Vector[String],
    ignoredFiles: Vector[String],
    ruleIgnoredFiles: Vector[String],
    gitignores: Map[String, String]
):
  /** What a walk of the written tree should return: the visible files and the `.gitignore` files themselves. */
  def expectedListing: Vector[String] = (files ++ gitignores.keys).sorted

/** Builds the tree the project-indexing benchmarks walk (#2420). Everything is derived from the shape and the seed with
  * a fixed generator, so the same arguments give the same tree on every JVM.
  */
object SyntheticProjectTree:

  private val Extensions   = Vector("scala", "ts", "md", "json", "txt")
  private val IgnoredNames = Vector("node_modules", "target", "dist", "build")
  private val RootRules    = "*.log\n"
  private val Stride       = 7919L

  /** SplitMix64: a small generator whose output depends only on its state, so a tree does not change between JVMs. */
  final private case class Rng(state: Long):

    def next: (Rng, Long) =
      val advanced = state + 0x9e3779b97f4a7c15L
      val mixed    = (advanced ^ (advanced >>> 30)) * 0xbf58476d1ce4e5b9L
      val finished = (mixed ^ (mixed >>> 27)) * 0x94d049bb133111ebL
      (Rng(advanced), finished ^ (finished >>> 31))

    def nextInt(bound: Int): (Rng, Int) =
      val (after, value) = next
      (after, ((value >>> 1) % bound).toInt)

    def nextDouble: (Rng, Double) =
      val (after, value) = next
      (after, (value >>> 11) * (1.0 / (1L << 53)))

  final private case class Grown(rng: Rng, tickets: Vector[Int], paths: Vector[String], depths: Vector[Int])

  def plan(shape: TreeShape, seed: Long): TreePlan =
    val (afterGrowth, paths) = grow(shape, Rng(seed))
    val counts               = filesPerDirectory(shape, afterGrowth)
    val ignored              = ignoredDirectoriesIn(shape, paths)
    TreePlan(
      directories = paths,
      files = paths.zip(counts).flatMap((path, count) => filesIn(path, count)),
      ignoredDirectories = ignored,
      ignoredFiles = ignored.flatMap(directory => ignoredFilesIn(shape, directory)),
      ruleIgnoredFiles = ruleIgnoredIn(shape, paths),
      gitignores = Map(".gitignore" -> RootRules)
    )

  private def ignoredDirectoriesIn(shape: TreeShape, paths: Vector[String]): Vector[String] =
    val spacing = paths.size / shape.ignoredRoots.max(1)
    (0 until shape.ignoredRoots).toVector.map(r => child(paths(r * spacing), IgnoredNames(r % IgnoredNames.size)))

  private def ignoredFilesIn(shape: TreeShape, directory: String): Vector[String] =
    (0 until shape.ignoredFilesPerRoot).toVector.map(j => child(directory, s"pkg-$j.js"))

  private def ruleIgnoredIn(shape: TreeShape, paths: Vector[String]): Vector[String] =
    (0 until shape.ruleIgnoredFiles).toVector.map(i => child(paths((i * Stride % paths.size).toInt), s"build-$i.log"))

  /** Writes the plan under `root`, then sets every directory's modification time to `modifiedAt`. The stamp goes last
    * because creating a file changes its directory's time, and an old time keeps `ProjectFileIndex` from treating a
    * directory as changed (its racy window is 2 s), so a revalidation needs no sleep.
    */
  def materialize(plan: TreePlan, root: Path, modifiedAt: Instant): Unit =
    val directories = plan.directories ++ plan.ignoredDirectories
    directories.foreach(relative => Files.createDirectories(root.resolve(relative)))
    (plan.files ++ plan.ignoredFiles ++ plan.ruleIgnoredFiles).foreach(relative =>
      Files.createFile(root.resolve(relative))
    )
    plan.gitignores.foreach((relative, content) => Files.writeString(root.resolve(relative), content))
    val stamp = FileTime.from(modifiedAt)
    directories.foreach(relative => Files.setLastModifiedTime(root.resolve(relative), stamp))

  private def child(parent: String, name: String): String =
    if parent.isEmpty then name else s"$parent/$name"

  private def filesIn(path: String, count: Int): Vector[String] =
    (0 until count).toVector.map(k => child(path, s"F$k.${Extensions(k % Extensions.size)}"))

  /** Directories attach to a parent chosen in proportion to one plus its children, so a few directories get many
    * subdirectories, as in the real trees; a parent already at `maxDepth` is passed over.
    */
  private def grow(shape: TreeShape, rng: Rng): (Rng, Vector[String]) =
    val start = Grown(rng, Vector(0), Vector(""), Vector(0))
    val grown = (1 until shape.directories).foldLeft(start)((state, index) => attach(shape, state, index))
    (grown.rng, grown.paths)

  private def attach(shape: TreeShape, state: Grown, index: Int): Grown =
    val (next, parent) = pickParent(shape, state, attempts = 64)
    val depth          = state.depths(parent) + 1
    val name           = if depth == 1 then s"module$index" else s"pkg$index"
    Grown(
      next,
      state.tickets :+ parent :+ index,
      state.paths :+ child(state.paths(parent), name),
      state.depths :+ depth
    )

  @tailrec
  private def pickParent(shape: TreeShape, state: Grown, attempts: Int): (Rng, Int) =
    val (next, pick) = state.rng.nextInt(state.tickets.size)
    val candidate    = state.tickets(pick)
    if state.depths(candidate) < shape.maxDepth then (next, candidate)
    else if attempts == 0 then (next, 0)
    else pickParent(shape, state.copy(rng = next), attempts - 1)

  /** Log-normal weights, turned into whole counts that add up to `visibleFiles` exactly. */
  private def filesPerDirectory(shape: TreeShape, rng: Rng): Vector[Int] =
    val (_, weights) = (0 until shape.directories).foldLeft((rng, Vector.empty[Double])) { (state, _) =>
      val (afterFirst, u1)  = state._1.nextDouble
      val (afterSecond, u2) = afterFirst.nextDouble
      val radius            = StrictMath.sqrt(-2.0 * StrictMath.log(1.0 - u1))
      val gaussian          = radius * StrictMath.cos(2.0 * StrictMath.PI * u2)
      (afterSecond, state._2 :+ StrictMath.exp(shape.filesSigma * gaussian))
    }
    apportion(weights, shape.visibleFiles)

  /** Largest-remainder rounding: every directory gets the floor of its share, and the files left over go one each to
    * the directories with the largest fractions.
    */
  private def apportion(weights: Vector[Double], total: Int): Vector[Int] =
    val sum       = weights.sum
    val exact     = weights.map(_ / sum * total)
    val floors    = exact.map(_.toInt)
    val remainder = total - floors.sum
    val ranked    = exact.zipWithIndex.map((value, index) => (value - value.floor, index))
    val bumped    = ranked.sortBy((fraction, index) => (-fraction, index)).take(remainder).map(_._2).toSet
    floors.zipWithIndex.map((count, index) => if bumped(index) then count + 1 else count)
