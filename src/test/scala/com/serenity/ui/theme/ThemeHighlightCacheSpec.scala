package com.serenity.ui.theme

import java.awt.Color

import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.SemanticToken
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Cache semantics for the per-row highlight memo (issue #1843): hits and misses both return exactly what
  * [[ThemeManager.computeHighlightLine]] would, the bound holds, and eviction is least-recently-used.
  */
class ThemeHighlightCacheSpec extends AnyFlatSpec with Matchers:

  private val theme = Theme.dark

  private val keywordTokens = Some(
    List(SemanticToken(line = 0, startCharacter = 0, length = 3, tokenType = "keyword", tokenModifiers = Set.empty))
  )

  private def highlightScala(cache: ThemeHighlightCache, line: String): List[StyledText] =
    cache.highlightLine(line, theme, Some(LanguageId.Scala), keywordTokens)

  "ThemeHighlightCache.highlightLine" should "return the memoized result for a repeated line and theme" in {
    val cache = ThemeHighlightCache()

    val first = highlightScala(cache, "val x = 1")

    highlightScala(cache, "val x = 1") should be theSameInstanceAs first
  }

  it should "return what computeHighlightLine returns, on a miss and on a hit" in {
    val cache = ThemeHighlightCache()
    val inputs = List(
      ("val x = 1", Some(LanguageId.Scala), keywordTokens),
      ("val x = 1", Some(LanguageId.Scala), None),
      ("# Heading with `code`", Some(LanguageId.Markdown), None),
      ("plain prose", None, None)
    )

    inputs.foreach {
      case (line, language, tokens) =>
        val expected = ThemeManager.computeHighlightLine(line, theme, language, tokens)
        cache.highlightLine(line, theme, language, tokens) shouldBe expected
        cache.highlightLine(line, theme, language, tokens) shouldBe expected
    }
  }

  it should "miss for a theme that shares another theme's name but differs in colours" in {
    val cache      = ThemeHighlightCache()
    val lensTheme  = theme.copy(background = new Color(1, 2, 3))
    val darkResult = cache.highlightLine("plain prose", theme)
    val lensResult = cache.highlightLine("plain prose", lensTheme)
    val lensExpect = ThemeManager.computeHighlightLine("plain prose", lensTheme, None, None)

    lensTheme.name shouldBe theme.name
    lensResult shouldBe lensExpect
    lensResult should not be darkResult
  }

  it should "keep a recently hit line when the cache overflows, evicting the least recently used one instead" in {
    val cache   = ThemeHighlightCache()
    val anchor  = highlightScala(cache, "val anchor = 0")
    val fillers = (1 until ThemeHighlightCache.MaxEntries).map(i => highlightScala(cache, s"val filler$i = $i"))

    highlightScala(cache, "val anchor = 0") should be theSameInstanceAs anchor
    highlightScala(cache, "val overflow = 1")

    highlightScala(cache, "val anchor = 0") should be theSameInstanceAs anchor
    highlightScala(cache, "val filler2 = 2") should be theSameInstanceAs fillers(1)
    highlightScala(cache, "val filler1 = 1") should not be theSameInstanceAs(fillers(0))
  }
