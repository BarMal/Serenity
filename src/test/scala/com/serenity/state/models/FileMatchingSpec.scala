package com.serenity.state.models

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicInteger

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** A query that extends the previous one looks only at the files that matched it, and the answer is always the one a
  * fresh match over every file gives (#1926).
  */
class FileMatchingSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  private val limit = 5

  private val corpus: Vector[Path] =
    (0 until 3000).map(index => Path.of(s"src/module$index/Service$index.scala")).toVector

  private def counted(calls: AtomicInteger): FileMatching.Scorer =
    (query, path) =>
      val _ = calls.incrementAndGet()
      FuzzyMatch.score(query, path)

  private def matchesOf(query: String, previous: Option[FileMatches] = None): FileMatches =
    FileMatching.ranked(corpus, query, previous, limit)._2

  private def scoreCalls(query: String, previous: FileMatches): Int =
    val calls = new AtomicInteger(0)
    FileMatching.ranked(corpus, query, Some(previous), limit, counted(calls))
    calls.get

  private def oracle(files: Vector[Path], query: String): Vector[Path] =
    files
      .flatMap(file => FuzzyMatch.score(query, FileFinder.slashed(file)).map(file -> _))
      .sortBy((file, score) => (-score, file.toString))
      .map(_._1)

  "A query extending the previous one" should "score only the files the previous query matched" in {
    val previous = matchesOf("mod9")

    previous.files.size should (be > 0 and be < corpus.size)
    scoreCalls("mod99", previous) shouldBe previous.files.size
  }

  it should "narrow regardless of case" in {
    val previous = matchesOf("mod9")

    scoreCalls("MOD91", previous) shouldBe previous.files.size
  }

  it should "narrow again from the narrowed set" in {
    val first  = matchesOf("mod9")
    val second = matchesOf("mod90", Some(first))

    second.files.size should be < first.files.size
    scoreCalls("mod905", second) shouldBe second.files.size
  }

  "A query that does not extend the previous one" should "score every file" in {
    val previous = matchesOf("mod99")

    scoreCalls("mod9", previous) shouldBe corpus.size
    scoreCalls("mod98", previous) shouldBe corpus.size
    scoreCalls("xmod99", previous) shouldBe corpus.size
    scoreCalls("", previous) shouldBe corpus.size
  }

  "Ranking from a previous match" should "give the best files, best first, as ranking every file does" in {
    val (narrowedBest, _) = FileMatching.ranked(corpus, "mod99", Some(matchesOf("mod9")), limit)

    narrowedBest shouldBe oracle(corpus, "mod99").take(limit)
  }

  private enum Edit:
    case Append(char: Char)
    case Backspace
    case Replace(char: Char)

  private val alphabet = Gen.oneOf("aAbB/._- ".toList)

  private val edits = Gen.oneOf[Edit](
    alphabet.map(Edit.Append(_)),
    Gen.const(Edit.Backspace),
    alphabet.map(Edit.Replace(_))
  )

  private def edited(query: String, edit: Edit): String =
    edit match
      case Edit.Append(char)  => query + char
      case Edit.Backspace     => query.dropRight(1)
      case Edit.Replace(char) => query.dropRight(1) + char

  private val randomCorpus: Gen[Vector[Path]] =
    Gen
      .listOf(Gen.choose(1, 12).flatMap(length => Gen.stringOfN(length, alphabet)))
      .map(_.filter(name => !name.startsWith("/") && !name.contains("//")).map(Path.of(_)).distinct.toVector)

  it should "match a fresh ranking of every file across typing, deleting and replacing" in
    forAll(randomCorpus, Gen.listOf(edits)) { (files, steps) =>
      val queries = steps.scanLeft("")(edited)
      queries.foldLeft(Option.empty[FileMatches]) { (previous, query) =>
        val (best, matches) = FileMatching.ranked(files, query, previous, limit)
        val expected        = oracle(files, query)
        best shouldBe expected.take(limit)
        matches.files.toSet shouldBe expected.toSet
        matches.query shouldBe query
        Some(matches)
      }
    }
