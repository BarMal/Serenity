package com.serenity.perf

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import com.serenity.perf.BenchmarkFixtures.loremIpsumProse
import com.serenity.rope.*
import com.serenity.state.models.{FindOptions, FindSearch}

/** The paths that read text boundaries through a per-thread `BreakIterator` and count words through a per-thread
  * `BreakIterator` in a rope's summaries (#2350, the `TextEditing`, `RopeCharacterSource` and `TextCounts` entries of
  * `PurityAllowlist`). A change to how those iterators are owned has to be judged on these rows, which isolate each
  * path instead of burying it in a whole frame:
  *
  *   - `text.boundary.word_motion` and `grapheme_motion`: boundary questions about one unchanged rope, the pattern a
  *     find scan or a run of cursor moves makes;
  *   - `text.boundary.keystroke_after_edit`: one edit to the rope, then the four boundary questions a keystroke asks
  *     about the new text, so the identity anchor misses every time;
  *   - `text.boundary.find_whole_word`: a whole-word find over a large document, which asks `isWholeWordRange` of every
  *     candidate;
  *   - `text.counts.rope_build` and `rope_edit`: the word and character counts a rope computes for its leaves and
  *     joins.
  *
  * Run just these, without a window: `sbt "Test/runMain com.serenity.perf.BoundaryCacheBenchmarks"`.
  */
private[perf] object BoundaryCacheBenchmarks:

  given Balance = Balance.default

  private val Warmups    = 3
  private val Iterations = 20
  private val Probes     = 64

  def benchmarks(): List[BenchmarkRunner.Benchmark] = boundaryBenchmarks() ++ findBenchmarks() ++ countBenchmarks()

  def main(args: Array[String]): Unit =
    BenchmarkRunner.printResults(BenchmarkRunner.runMatching(args.toList, benchmarks()))

  /** A cluster is more than one char, so a boundary question next to it cannot be answered by the plain-neighbour fast
    * path and reaches the `BreakIterator`: e with a combining acute, a thumbs-up with a skin tone, and the Devanagari
    * syllable ni.
    */
  private val Clusters = Vector("e\u0301", "\uD83D\uDC4D\uD83C\uDFFD", "\u0928\u093F")

  /** The prose with a cluster after every sixth word, and where each cluster starts and how long it is. */
  private def proseWithClusters(paragraphs: Int): (String, Vector[(Int, Int)]) =
    val words    = loremIpsumProse(paragraphs).split(' ')
    val text     = new StringBuilder
    val clusters = Vector.newBuilder[(Int, Int)]
    words.zipWithIndex.foreach { (word, index) =>
      if index > 0 then text.append(' ')
      text.append(word)
      if index % 6 == 0 then
        val cluster = Clusters((index / 6) % Clusters.length)
        clusters += ((text.length, cluster.length))
        text.append(cluster)
    }
    (text.toString, clusters.result())

  private def boundaryBenchmarks(): List[BenchmarkRunner.Benchmark] =
    val (text, allClusters) = proseWithClusters(paragraphs = 400)
    val rope                = Rope(text)
    val stride              = allClusters.length / Probes
    val clusters            = Vector.tabulate(Probes)(index => allClusters(index * stride))
    def wordMotion(): Int =
      clusters.foldLeft(0)((sum, cluster) =>
        sum + rope.nextWordBoundary(cluster._1) + rope.previousWordBoundary(cluster._1)
      )
    def graphemeMotion(): Int =
      clusters.foldLeft(0)((sum, cluster) =>
        sum + rope.nextGraphemeBoundary(cluster._1) + rope.previousGraphemeBoundary(cluster._1 + cluster._2)
      )
    def crossesWholeClusters(): Boolean =
      clusters.forall((start, length) =>
        rope.nextGraphemeBoundary(start) == start + length && rope.previousGraphemeBoundary(start + length) == start
      )

    val (typedAt, clusterLength) = allClusters(allClusters.length / 2)
    val typing                   = new AtomicReference[(Rope, Int)]((rope, typedAt))
    def keystroke(): Int =
      val (current, cursor) = typing.get
      val edited            = current.insert(cursor, if cursor % 2 == 0 then "a" else " ").getOrElse(current)
      val next              = cursor + 1
      typing.set((edited, next))
      edited.previousWordBoundary(next) + edited.nextWordBoundary(next) +
        edited.previousGraphemeBoundary(next + clusterLength) + edited.nextGraphemeBoundary(next)

    List(
      BenchmarkRunner.Benchmark(
        "text.boundary.word_motion",
        Warmups,
        Iterations,
        () => assert(wordMotion() > 0, "word motion found no boundaries"),
        () => wordMotion()
      ),
      BenchmarkRunner.Benchmark(
        "text.boundary.grapheme_motion",
        Warmups,
        Iterations,
        () => assert(crossesWholeClusters(), "a grapheme question did not cross a whole cluster"),
        () => graphemeMotion()
      ),
      BenchmarkRunner.Benchmark(
        "text.boundary.keystroke_after_edit",
        Warmups,
        Iterations,
        () => assert(keystroke() > 0, "keystroke found no boundaries"),
        () => keystroke()
      )
    )

  private def findBenchmarks(): List[BenchmarkRunner.Benchmark] =
    val document  = Rope(loremIpsumProse(paragraphs = 3000))
    val wholeWord = FindOptions(wholeWord = true)
    def search()  = FindSearch.search(document, "dolor", wholeWord, anchor = 0)

    List(
      BenchmarkRunner.Benchmark(
        "text.boundary.find_whole_word",
        Warmups,
        Iterations,
        () => assert(search().results.nonEmpty, "whole-word find matched nothing"),
        () => search()
      )
    )

  private def countBenchmarks(): List[BenchmarkRunner.Benchmark] =
    val text   = loremIpsumProse(paragraphs = 400)
    val base   = Rope(text)
    val counts = base.wordCount
    val edits  = new AtomicInteger(0)
    def edit(): Int =
      val offset = 3 + (edits.getAndIncrement() * 7919) % (base.weight - 8)
      base.insert(offset, "ab ").map(_.wordCount).getOrElse(-1)

    List(
      BenchmarkRunner.Benchmark(
        "text.counts.rope_build",
        Warmups,
        Iterations,
        () => assert(Rope(text).wordCount == counts, "a rebuilt rope counted differently"),
        () => Rope(text).wordCount
      ),
      BenchmarkRunner.Benchmark(
        "text.counts.rope_edit",
        Warmups,
        Iterations,
        () => assert(edit() == counts + 1, "an inserted word did not add one to the count"),
        () => edit()
      )
    )
