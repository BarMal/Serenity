package com.serenity

import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.{CursorPosition, FindResult, FindResultSet, FindSearch}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FindResultSetSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "FindResultSet" should "wrap selected indexes through available results" in {
    val results = Vector(FindResult(0, 0), FindResult(2, 3), FindResult(4, 6))

    FindResultSet.normalized("needle", results, requestedIndex = 4).currentIndex.shouldBe(1)
    FindResultSet.normalized("needle", results, requestedIndex = -1).currentIndex.shouldBe(2)
  }

  it should "normalize empty and no-match states to index zero" in {
    FindResultSet.normalized("", Vector.empty, requestedIndex = 3).shouldBe(FindResultSet.empty)
    FindResultSet.normalized("missing", Vector.empty, requestedIndex = 3).currentIndex.shouldBe(0)
  }

  it should "describe the selected result for result workflow rendering" in {
    val resultSet = FindResultSet.normalized(
      "needle",
      Vector(FindResult(2, 4), FindResult(5, 8), FindResult(8, 12)),
      requestedIndex = 1
    )

    resultSet.selectedResult.shouldBe(Some(FindResult(5, 8)))
    resultSet.selectionSummary.shouldBe("3 matches, 2/3 at 6:9")
  }

  it should "window visible results around the selected result" in {
    val results   = (0 until 10).map(line => FindResult(line, 0)).toVector
    val resultSet = FindResultSet.normalized("needle", results, requestedIndex = 7)

    resultSet
      .visibleResults(maxResults = 5)
      .shouldBe(
        List(
          FindResult(5, 0) -> 5,
          FindResult(6, 0) -> 6,
          FindResult(7, 0) -> 7,
          FindResult(8, 0) -> 8,
          FindResult(9, 0) -> 9
        )
      )
  }

  it should "return no visible results when the result window has no space" in {
    val resultSet = FindResultSet.normalized("needle", Vector(FindResult(0, 0)), requestedIndex = 0)

    resultSet.visibleResults(maxResults = 0).shouldBe(Nil)
  }

  it should "preserve non-overlapping match positions from rope search results" in {
    val content = Rope("aaaa")
    val results = content.searchAll("aa").map(offset => FindResult(0, offset)).toVector

    FindResultSet
      .normalized("aa", results, requestedIndex = 1)
      .shouldBe(
        FindResultSet.normalized("aa", Vector(FindResult(0, 0), FindResult(0, 2)), 1)
      )
  }

  it should "pick the first result at or after a caret, wrapping to the first when none follows" in {
    val results = Vector(FindResult(0, 0), FindResult(2, 3), FindResult(4, 6))

    FindResultSet.indexAtOrAfter(results, CursorPosition(1, 9)).shouldBe(1)
    FindResultSet.indexAtOrAfter(results, CursorPosition(2, 3)).shouldBe(1)
    FindResultSet.indexAtOrAfter(results, CursorPosition(4, 7)).shouldBe(0)
    FindResultSet.indexAtOrAfter(Vector.empty, CursorPosition(4, 7)).shouldBe(0)
  }

  it should "pick the first result strictly after a caret, wrapping to the first when none follows" in {
    val results = Vector(FindResult(0, 0), FindResult(2, 3), FindResult(4, 6))

    FindResultSet.indexAfter(results, CursorPosition(0, 0)).shouldBe(1)
    FindResultSet.indexAfter(results, CursorPosition(2, 4)).shouldBe(2)
    FindResultSet.indexAfter(results, CursorPosition(4, 6)).shouldBe(0)
  }

  "FindSearch.stillMatches" should "reject a stored result once its text no longer equals the query" in {
    val content = Rope("needle\nnoodle")

    FindSearch.stillMatches(content, "needle", FindResult(0, 0)).shouldBe(true)
    FindSearch.stillMatches(content, "needle", FindResult(1, 0)).shouldBe(false)
  }

  it should "reject a stored column past the end of its line rather than matching wherever the offset clamps to" in {
    val content = Rope("ab\nneedle")

    FindSearch.stillMatches(content, "\nneedle", FindResult(0, 2)).shouldBe(true)
    FindSearch.stillMatches(content, "\nneedle", FindResult(0, 5)).shouldBe(false)
  }

end FindResultSetSpec
