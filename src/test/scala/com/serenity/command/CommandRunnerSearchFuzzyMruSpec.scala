package com.serenity.command

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** issue #1048: fuzzy subsequence scoring (replacing token prefix/contains matching wholesale) and MRU-weighted
  * palette ranking.
  */
class CommandRunnerSearchFuzzyMruSpec extends AnyFlatSpec with Matchers:

  private def testCommand(name: String, label: String): Command =
    Command.typed(name, s"$label test command", CommandIntent.Edit(EditIntent.Undo), label = label)

  "fuzzyScore" should "score an exact match highest" in {
    CommandRunnerSearch.fuzzyScore("line", "line") shouldBe Some(1000.0)
  }

  it should "score a prefix match above a contiguous substring match" in {
    val prefix    = CommandRunnerSearch.fuzzyScore("line", "line-numbers").getOrElse(fail("expected a match"))
    val substring = CommandRunnerSearch.fuzzyScore("line", "toggle-line-numbers").getOrElse(fail("expected a match"))
    prefix should be > substring
  }

  it should "score a word-boundary substring match above a mid-word substring match" in {
    val boundary = CommandRunnerSearch.fuzzyScore("line", "toggle-line-numbers").getOrElse(fail("expected a match"))
    val midWord  = CommandRunnerSearch.fuzzyScore("line", "outline-panel").getOrElse(fail("expected a match"))
    boundary should be > midWord
  }

  it should "still match (at a lower score) a scattered, non-contiguous subsequence" in {
    // "l"(1), "i"(2), "n"(4), "e"(5) appear in that order in "aligned" but never contiguously as "line".
    CommandRunnerSearch.fuzzyScore("line", "aligned") shouldBe defined
    val scattered = CommandRunnerSearch.fuzzyScore("line", "aligned").get
    val boundary   = CommandRunnerSearch.fuzzyScore("line", "toggle-line-numbers").get
    scattered should be < boundary
  }

  it should "return None when the term is not even a subsequence of the target" in {
    CommandRunnerSearch.fuzzyScore("xyz", "toggle-line-numbers") shouldBe None
  }

  "isStrongCommandMatch" should "treat a word-boundary substring hit in the name as strong" in {
    val command = testCommand("toggle-line-numbers", "Toggle Line Numbers")
    CommandRunnerSearch.isStrongCommandMatch(command, "line") shouldBe true
  }

  it should "not treat a mid-word scattered/substring hit as strong" in {
    val command = testCommand("toggle-outline-panel", "Toggle Outline")
    CommandRunnerSearch.isStrongCommandMatch(command, "line") shouldBe false
  }

  "CommandSearcher" should "rank commands whose name contains the query as a word above unrelated commands" in {
    val lineNumbers = testCommand("toggle-line-numbers", "Toggle Line Numbers")
    val lineWrap    = testCommand("toggle-line-wrap", "Toggle Line Wrap")
    val unrelated   = testCommand("save-current-file", "Save")
    val searcher    = new CommandSearcher(List(unrelated, lineNumbers, lineWrap))

    val results = searcher.search("line", maxResults = 10)

    results.take(2).toSet shouldBe Set(lineNumbers, lineWrap)
    results should not contain unrelated
  }

  "CommandRunner.recordCommandUsage" should "give the most recently recorded command the highest generation" in {
    val runner = CommandRunner.empty.recordCommandUsage("a").recordCommandUsage("b").recordCommandUsage("a")

    runner.commandUsage("a") should be > runner.commandUsage("b")
  }

  it should "re-rank equally-relevant search results so a recently used command floats up" in {
    val commandA = testCommand("test-alpha-widget", "Alpha Widget")
    val commandB = testCommand("test-beta-widget", "Beta Widget")
    given CommandRegistry = CommandRegistry(List(commandA, commandB))

    val used = CommandRunner.empty
      .recordCommandUsage(commandB.name)
      .updateSearchTerm("widget")

    used.filteredCommands.indexOf(commandB) should be < used.filteredCommands.indexOf(commandA)
  }
