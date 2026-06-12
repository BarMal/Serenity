package com.serenity

import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.{FindResult, FindResultSet}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FindResultSetSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "FindResultSet" should "wrap selected indexes through available results" in {
    val results = List(FindResult(0, 0), FindResult(2, 3), FindResult(4, 6))

    FindResultSet.normalized("needle", results, requestedIndex = 4).currentIndex.shouldBe(1)
    FindResultSet.normalized("needle", results, requestedIndex = -1).currentIndex.shouldBe(2)
  }

  it should "normalize empty and no-match states to index zero" in {
    FindResultSet.normalized("", Nil, requestedIndex = 3).shouldBe(FindResultSet.empty)
    FindResultSet.normalized("missing", Nil, requestedIndex = 3).currentIndex.shouldBe(0)
  }

  it should "describe the selected result for result workflow rendering" in {
    val resultSet = FindResultSet.normalized(
      "needle",
      List(FindResult(2, 4), FindResult(5, 8), FindResult(8, 12)),
      requestedIndex = 1
    )

    resultSet.selectedResult.shouldBe(Some(FindResult(5, 8)))
    resultSet.selectionSummary.shouldBe("3 matches, 2/3 at 6:9")
  }

  it should "window visible results around the selected result" in {
    val results   = (0 until 10).map(line => FindResult(line, 0)).toList
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
    val resultSet = FindResultSet.normalized("needle", List(FindResult(0, 0)), requestedIndex = 0)

    resultSet.visibleResults(maxResults = 0).shouldBe(Nil)
  }

  it should "preserve non-overlapping match positions from rope search results" in {
    val content = Rope("aaaa")
    val results = content.searchAll("aa").map(offset => FindResult(0, offset))

    FindResultSet
      .normalized("aa", results, requestedIndex = 1)
      .shouldBe(
        FindResultSet.normalized("aa", List(FindResult(0, 0), FindResult(0, 2)), 1)
      )
  }

end FindResultSetSpec
