package com.serenity.io

import java.nio.file.Path
import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.perf.{SyntheticProjectTree, TreeShape}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The warm `index.walk` row (#2420) measures a revalidation only if the synthetic tree's old directory times keep the
  * index from reading anything again; this is the check that they do.
  */
class SyntheticTreeRevalidationSpec extends AnyFlatSpec with Matchers:

  private class Reads:
    private val seen = new AtomicReference(Vector.empty[Path])

    val reader: DirectoryReader = directory =>
      val _ = seen.updateAndGet(_ :+ directory)
      ProjectFileWalker.readDirectory(directory)

    def count: Int    = seen.get.size
    def clear(): Unit = seen.set(Vector.empty)

  "a materialized synthetic tree" should "be read in full once and then revalidated without reading a directory" in {
    val plan  = SyntheticProjectTree.plan(TreeShape.Small, 20260430L)
    val root  = TestTemp.directory("synthetic-revalidation")
    val reads = new Reads
    SyntheticProjectTree.materialize(plan, root, Instant.now().minus(1, ChronoUnit.DAYS))
    val index = ProjectFileIndex.emptyReading(reads.reader, IO.pure(Vector.empty), firstPartialAt = 4096)

    index.listing(root, _ => IO.unit).unsafeRunSync()
    reads.count shouldBe plan.directories.size
    reads.clear()
    index.listing(root, _ => IO.unit).unsafeRunSync()

    reads.count shouldBe 0
  }
