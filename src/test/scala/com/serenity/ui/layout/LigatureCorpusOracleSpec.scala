package com.serenity.ui.layout

import java.awt.Font
import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.{Random, Using}

import com.serenity.ui.fonts.FontLoader
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The safety property behind counting a line as one row in a font set with ligatures (#2447 B2): whenever the
  * certificate calls a line one row at a width, wrapping it at that width gives one row. The certificate is not proven
  * exact for such a font, so this runs it against a large corpus and the unguarded wrap is the oracle: this
  * repository's own sources, operator-dense code of many languages generated from their token lists, and random
  * printable text biased to symbols. A line is tried at widths either side of its own.
  */
class LigatureCorpusOracleSpec extends AnyFlatSpec with Matchers with OptionValues:

  private val frc    = TextLayoutSnapshot.defaultFontRenderContext()
  private val config = FontLoader.FontConfig()

  private def ligatureFont(sizePt: Float): Font =
    FontLoader.previewCodeFont(config.copy(fontSize = sizePt, enableLigatures = true))

  private def widthsAround(text: String, font: Font): List[Int] =
    val total = font.createGlyphVector(frc, text).getGlyphPosition(text.length).getX.toFloat
    List(math.floor(total).toInt - 1, math.floor(total).toInt, math.ceil(total).toInt, math.ceil(total).toInt + 1)
      .map(math.max(1, _))
      .distinct

  private def wrappedRows(text: String, widthPx: Int, font: Font, cache: WrappedLineCache): Int =
    TextLayoutSnapshot
      .boundedVisualLinesForText(
        text,
        0,
        widthPx,
        font,
        cellMetricsOverride = Some(CellMetrics.fromFont(font)),
        wrapCache = cache
      )
      .length
      .max(1)

  /** Lines the certificate calls one row that the wrap does not, with the width, as `size pt, width: line`. */
  private def disagreements(corpus: Seq[String], sizes: List[Float]): List[String] =
    // A line repeated in the corpus (a closing brace, an import) is counted once: the same text gives the same answer.
    val lines = corpus.distinct
    sizes.flatMap { size =>
      val font  = ligatureFont(size)
      val table = LigatureCertificate.of(font, frc).value
      val cache = WrappedLineCache.bounded()
      lines.iterator
        .flatMap { text =>
          widthsAround(text, font).collect {
            case width if table.fitsOneRow(text, width) && wrappedRows(text, width, font, cache) != 1 =>
              s"$size pt, $width px: $text"
          }
        }
        .take(5)
        .toList
    }

  private val sourceRoot = Paths.get("src")

  private def repositoryLines(): Vector[String] =
    val files = Using.resource(Files.walk(sourceRoot))(_.iterator.asScala.filter(isSource).toVector.sortBy(_.toString))
    files.zipWithIndex.flatMap { (file, index) =>
      Files.readAllLines(file).asScala.zipWithIndex.collect { case (line, at) if (at + index) % 7 == 0 => line }
    }

  private def isSource(path: Path): Boolean =
    Files.isRegularFile(path) && path.toString.endsWith(".scala")

  private val operators = Map(
    "scala" -> List(
      "=>",
      "->",
      "<-",
      "::",
      ":::",
      "<:",
      ">:",
      "|>",
      "<|",
      "++",
      "--",
      "+=",
      "-=",
      "==",
      "!=",
      "<=",
      ">=",
      "&&",
      "||",
      "=:=",
      "<:<"
    ),
    "rust" -> List("=>", "->", "::", "..=", "..", "&&", "||", "?", "!=", "<=", ">=", "<<=", ">>=", "|>", "'a"),
    "go"   -> List(":=", "<-", "...", "&&", "||", "!=", "<=", ">=", "<<", ">>", "&^", "++", "--"),
    "c" -> List("->", "++", "--", "<<", ">>", "<<=", ">>=", "&&", "||", "==", "!=", "<=", ">=", "::", "/*", "*/", "//"),
    "javascript" -> List("=>", "===", "!==", "?.", "??", "??=", "&&=", "||=", "**", "...", "</", "/>", "<!--", "-->"),
    "haskell" -> List(
      "<$>",
      "<*>",
      ">>=",
      "=<<",
      ">=>",
      "<=<",
      "->",
      "<-",
      "=>",
      "::",
      "<>",
      "++",
      "!!",
      "\\\\",
      ".|.",
      "&&&",
      "***",
      "|||",
      "<|>"
    ),
    "ocaml"    -> List("|>", "@@", "->", "::", ":=", "<>", "<-", "!=", "==", "||", "&&", "(*", "*)"),
    "elixir"   -> List("|>", "<>", "++", "--", "->", "<-", "=~", "==", "===", "!=", "&&", "||", "..", "<<<", ">>>"),
    "lisp"     -> List("'(", "`(", ",@", "#'", "(lambda", "(defun", ")))", "((", "))"),
    "shell"    -> List("&&", "||", ">>", "<<", "2>&1", "|&", ";;", "$(", "${", "#!/bin/sh", "\\\\", "<<<"),
    "sql"      -> List("<>", "!=", "<=", ">=", "||", "::", "->", "->>", "#>", "@>", "<@", "--", "/*", "*/"),
    "markup"   -> List("<!--", "-->", "</", "/>", "<?", "?>", "<![CDATA[", "]]>", "&amp;", "&lt;", "&gt;", "==", "=\""),
    "json"     -> List("{\"", "\":", "\",", "[{", "}]", "null", "true", "false", ":{", "},"),
    "markdown" -> List("# ", "## ", "---", "***", "```", "- [ ]", "- [x]", "=>", "->", "<!--", "-->", "|---|", "> "),
    "latex"    -> List("\\\\", "\\begin{", "\\end{", "$$", "\\frac{", "\\leq", "\\geq", "->", "=>", "&=", "\\%", "~")
  )

  private val words = List(
    "value",
    "x",
    "items",
    "result",
    "foo",
    "bar",
    "map",
    "filter",
    "Int",
    "String",
    "fn",
    "let",
    "if",
    "0",
    "42",
    "_",
    "i",
    "n"
  )

  private def languageLines(): Vector[String] =
    val random = Random(11)
    operators.values.toVector.flatMap { tokens =>
      Vector.fill(1500) {
        val count = 1 + random.nextInt(24)
        (0 until count)
          .map { _ =>
            val token =
              if random.nextInt(3) == 0 then words(random.nextInt(words.size)) else tokens(random.nextInt(tokens.size))
            if random.nextInt(4) == 0 then token else token + " "
          }
          .mkString
          .stripTrailing()
      }
    }

  private val symbols = "=<>-!/*+&|:.~#_^%?@\\;,()[]{}\"'`$"

  private def fuzzLines(count: Int): Vector[String] =
    val random = Random(2025)
    Vector.fill(count) {
      val length = random.nextInt(301)
      String(
        Array.fill(length)(
          if random.nextInt(10) < 6 then symbols(random.nextInt(symbols.length))
          else (0x20 + random.nextInt(0x5f)).toChar
        )
      )
    }

  "The certificate for a font set with ligatures" should "never call one row a line of this repository's sources that the wrap wraps" in {
    val lines = repositoryLines()
    lines.size should be > 10000
    disagreements(lines, List(12.0f, 16.0f)) shouldBe Nil
  }

  it should "never call one row a line of operator-dense code in many languages that the wrap wraps" in {
    disagreements(languageLines(), List(11.0f, 12.0f, 14.0f, 18.0f)) shouldBe Nil
  }

  it should "never call one row a random printable line that the wrap wraps" in {
    disagreements(fuzzLines(100000), List(12.0f)) shouldBe Nil
  }

  it should "call one row a good share of the lines the wrap puts on one row, so the corpus exercises it" in {
    val font  = ligatureFont(12.0f)
    val table = LigatureCertificate.of(font, frc).value
    val lines = languageLines()
    lines.count(table.fitsOneRow(_, 1472)) should be > lines.size / 2
  }
