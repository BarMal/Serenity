package com.serenity.perf

import java.nio.file.Path
import java.time.Instant
import java.time.temporal.ChronoUnit

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.io.{ProjectFileIndex, ProjectFileListing}

/** What opening the "Go to File" finder on a large project costs (#2420): `ProjectFileIndex.listing` over the synthetic
  * tree, from a fresh index (the first open, a full walk) and from one that has already walked it (every later open, a
  * revalidation). The tree is written the first time a row needs it, so a run that names other prefixes does not pay
  * for 100,000 files. Warm-up and iteration counts are the runner's mechanics, not targets; the rows are judged by the
  * existing paired-ratio gate. Run just these with `sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks
  * index.walk."`.
  */
private[perf] object IndexWalkBenchmarks:

  private val Seed           = 20260430L
  private val Warmups        = 1
  private val ColdIterations = 5
  private val WarmIterations = 20

  final private case class Fixture(root: Path, expected: Set[Path])

  def benchmarks(shape: TreeShape = TreeShape.Monorepo100k, label: String = "100k"): List[BenchmarkRunner.Benchmark] =
    lazy val fixture = written(shape)
    lazy val warmed  = walked(fixture.root)
    List(
      BenchmarkRunner.Benchmark(
        s"index.walk.cold.$label",
        Warmups,
        ColdIterations,
        () => verify(emptyIndex(), fixture),
        () => listing(emptyIndex(), fixture.root)
      ),
      BenchmarkRunner.Benchmark(
        s"index.walk.warm.$label",
        Warmups,
        WarmIterations,
        () => verify(warmed, fixture),
        () => listing(warmed, fixture.root)
      )
    )

  /** No global excludes file, so the walk does not depend on the machine's git configuration. */
  private def emptyIndex(): ProjectFileIndex = ProjectFileIndex.empty(globalExcludes = IO.pure(Vector.empty))

  private def listing(index: ProjectFileIndex, root: Path): ProjectFileListing =
    index.listing(root, _ => IO.unit).unsafeRunSync()

  private def walked(root: Path): ProjectFileIndex =
    val index = emptyIndex()
    val _     = listing(index, root)
    index

  private def written(shape: TreeShape): Fixture =
    val plan = SyntheticProjectTree.plan(shape, Seed)
    val root = TestTemp.directory("index-walk")
    SyntheticProjectTree.materialize(plan, root, Instant.now().minus(1, ChronoUnit.DAYS))
    Fixture(root, plan.expectedListing.map(Path.of(_)).toSet)

  private def verify(index: ProjectFileIndex, fixture: Fixture): Unit =
    val result = listing(index, fixture.root)
    assert(!result.truncated, "the walk stopped before the end of the tree")
    assert(result.files.toSet == fixture.expected, "the walk did not list exactly the plan's visible files")
