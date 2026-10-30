package com.serenity.perf

import java.nio.file.{Files, Path}
import java.time.Instant
import java.time.temporal.ChronoUnit

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.io.ProjectFileIndex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The tree the project-indexing benchmarks walk (#2420). The sizes come from that issue (100,000 files in 10,000
  * directories). Its shape is checked against four real trees measured on 2026-10-30, which are the basis of every
  * range below, not a performance target: this repository's `src` (2,716 files in 141 directories), `/usr/lib`
  * (16,064 in 1,506), `/usr/share` (26,637 in 2,912) and `/usr/include` (4,318 in 214). Across them the mean file depth
  * was 2.1 to 5.9, the deepest file 6 to 9 levels down, the median files per directory 1 to 5 against means of 9 to 20,
  * and the fullest directory held 549 to 1,341 files.
  */
class SyntheticProjectTreeSpec extends AnyFlatSpec with Matchers:

  private val seed  = 20260430L
  private val shape = TreeShape.Monorepo100k
  private val full  = SyntheticProjectTree.plan(shape, seed)

  private def depth(path: String): Int = path.count(_ == '/')

  /** Whole seconds, since not every file system keeps a modification time finer than that. */
  private def aDayAgo(): Instant = Instant.now().minusSeconds(86_400).truncatedTo(ChronoUnit.SECONDS)

  "SyntheticProjectTree.plan" should "be the same tree for the same seed and a different one for another" in {
    SyntheticProjectTree.plan(shape, seed) shouldBe full
    SyntheticProjectTree.plan(shape, seed + 1) should not be full
  }

  it should "have the issue's sizes: 100,000 files in 10,000 directories" in {
    full.files.size shouldBe 100_000
    full.directories.size shouldBe 10_000
  }

  it should "list no file or directory twice, and put every file in a directory the plan has" in {
    full.files.distinct.size shouldBe full.files.size
    full.directories.distinct.size shouldBe full.directories.size
    val directories = full.directories.toSet
    full.files.forall(file => directories(file.take(file.lastIndexOf('/').max(0)))) shouldBe true
  }

  it should "reach no deeper than the real trees did" in {
    full.files.map(depth).max should be <= 9
  }

  it should "put files at about the depth the real trees did (mean 2.1 to 5.9)" in {
    val mean = full.files.map(depth).sum.toDouble / full.files.size
    mean should (be >= 2.0 and be <= 6.0)
  }

  it should "have the real trees' lopsided files per directory: a median under half the mean, a fullest directory far above it" in {
    val byDirectory  = full.files.groupBy(file => file.take(file.lastIndexOf('/').max(0)))
    val perDirectory = byDirectory.values.map(_.size).toVector.sorted
    val mean         = full.files.size.toDouble / full.directories.size
    perDirectory(perDirectory.size / 2).toDouble should be <= mean / 2
    perDirectory.last.toDouble should be >= mean * 10
  }

  it should "include ignored directories and ignored files the walker has to skip, none of them among the visible files" in {
    full.ignoredDirectories should not be empty
    full.ignoredFiles should not be empty
    val names = full.ignoredDirectories.map(_.split('/').last).toSet
    names.subsetOf(Set("node_modules", "target", "dist", "build")) shouldBe true
    full.files.exists(file => full.ignoredDirectories.exists(ignored => file.startsWith(ignored + "/"))) shouldBe false
  }

  it should "carry a .gitignore at the root and files that its rules exclude" in {
    full.gitignores.keySet should contain(".gitignore")
    full.ruleIgnoredFiles should not be empty
    full.ruleIgnoredFiles.forall(_.endsWith(".log")) shouldBe true
  }

  "SyntheticProjectTree.materialize" should "write a tree whose listing by the project index is exactly the plan's visible files" in {
    val small = SyntheticProjectTree.plan(TreeShape.Small, seed)
    val root  = TestTemp.directory("synthetic-tree")

    SyntheticProjectTree.materialize(small, root, aDayAgo())

    val listing = ProjectFileIndex.empty().listing(root, _ => IO.unit).unsafeRunSync()
    listing.files.toSet shouldBe small.expectedListing.map(Path.of(_)).toSet
    listing.truncated shouldBe false
  }

  it should "leave every directory's modification time at the time it was given, so a later check sees nothing changed" in {
    val small = SyntheticProjectTree.plan(TreeShape.Small, seed)
    val root  = TestTemp.directory("synthetic-tree-mtime")
    val old   = aDayAgo()

    SyntheticProjectTree.materialize(small, root, old)

    val directories = (small.directories ++ small.ignoredDirectories).map(relative => root.resolve(relative))
    val modified    = directories.map(directory => Files.getLastModifiedTime(directory).toMillis)

    modified.distinct shouldBe Vector(old.toEpochMilli)
  }
