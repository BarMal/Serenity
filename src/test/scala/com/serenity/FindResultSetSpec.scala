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

  it should "preserve overlapping match positions from rope search results" in {
    val content = Rope("aaaa")
    val results = content.searchAll("aa").map(offset => FindResult(0, offset))

    FindResultSet
      .normalized("aa", results, requestedIndex = 2)
      .shouldBe(
        FindResultSet.normalized("aa", List(FindResult(0, 0), FindResult(0, 1), FindResult(0, 2)), 2)
      )
  }

end FindResultSetSpec
