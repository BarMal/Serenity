package com.serenity.text

/** As-you-type punctuation autoformatting: straight quotes become curly, `--` an en dash and `---` an em dash (the
  * SmartyPants rule), and three periods a true ellipsis -- the same conveniences prose-writing tools like Neo offer,
  * gated behind `languageToolsConfig.smartPunctuationEnabled`. The caller decides where it applies (prose buffers, and
  * never inside Markdown code); this object only knows the caret's line.
  */
object SmartPunctuation:

  private val EmDash           = "—"
  private val EnDash           = "–"
  private val Ellipsis         = "…"
  private val LeftDoubleQuote  = "“"
  private val RightDoubleQuote = "”"
  private val LeftSingleQuote  = "‘"
  private val RightSingleQuote = "’"

  private val RuleRowCharacters = Set('-', '|', ':', ' ', '\t')

  /** The replacement for `typed`, given the caret's line up to the caret. `None` means "insert `typed` unchanged".
    * Otherwise, the first element of the pair is how many of the trailing characters of `precedingLineText` to replace
    * (counting backward from the cursor), and the second is the full replacement text -- `typed` itself is never
    * inserted raw once a rule fires, it's folded into the replacement.
    */
  def replacementFor(typed: Char, precedingLineText: String): Option[(Int, String)] =
    typed match
      case '-' if isRuleRowSoFar(precedingLineText)  => None
      case '-' if precedingLineText.endsWith(EnDash) => Some((1, EmDash))
      case '-' if precedingLineText.endsWith("-")    => Some((1, EnDash))
      case '.' if precedingLineText.endsWith("..")   => Some((2, Ellipsis))
      case '"'  => Some((0, if opensQuote(precedingLineText) then LeftDoubleQuote else RightDoubleQuote))
      case '\'' => Some((0, if opensQuote(precedingLineText) then LeftSingleQuote else RightSingleQuote))
      case digit if digit.isDigit && opensElision(precedingLineText) => Some((1, RightSingleQuote + digit))
      case _                                                         => None

  private val StraightForms: Map[Char, String] = Map(
    LeftDoubleQuote.charAt(0)  -> "\"",
    RightDoubleQuote.charAt(0) -> "\"",
    LeftSingleQuote.charAt(0)  -> "'",
    RightSingleQuote.charAt(0) -> "'",
    Ellipsis.charAt(0)         -> "...",
    EnDash.charAt(0)           -> "--",
    EmDash.charAt(0)           -> "---"
  )

  /** What typing `text` one character at a time would have produced: the whole-text form of [[replacementFor]], used
    * when compiling a manuscript. Code spans are left alone, and applying it twice changes nothing.
    */
  def educate(text: String): String =
    educateTagged(text.toVector.map(_ -> false), identity).map(_._1).mkString

  /** Undoes [[educate]] on ASCII text: curly quotes go straight, `…` becomes `...`, and the dashes become the `--` and
    * `---` that type them. Code spans are left alone.
    */
  def straighten(text: String): String =
    straightenTagged(text.toVector.map(_ -> false), identity).map(_._1).mkString

  /** [[educate]] over characters that each carry a tag, such as the style of the run they came from, so that a dash
    * typed across two runs still joins up. Characters whose tag is `verbatim` are copied unchanged; a replacement takes
    * the tag of the character that triggered it.
    */
  def educateTagged[A](chars: Vector[(Char, A)], verbatim: A => Boolean): Vector[(Char, A)] =
    chars.foldLeft(Education.start[A])((education, tagged) => education.next(tagged, verbatim)).output

  def straightenTagged[A](chars: Vector[(Char, A)], verbatim: A => Boolean): Vector[(Char, A)] =
    chars
      .foldLeft((Vector.empty[(Char, A)], 0)) {
        case ((output, backticks), (char, tag)) =>
          val straight  = if verbatim(tag) || backticks % 2 == 1 then None else StraightForms.get(char)
          val nextTicks = if char == '\n' then 0 else if char == '`' then backticks + 1 else backticks
          (output ++ straight.fold(Vector(char -> tag))(_.toVector.map(_ -> tag)), nextTicks)
      }
      ._1

  /** The state of [[educateTagged]] partway through its input. [[replacementFor]] reads only the last two characters of
    * the line and whether the whole line is a rule row so far, so the fold hands it that much rather than rebuilding
    * the line for every character, which would make a long paragraph quadratic.
    */
  final private case class Education[A](
      output: Vector[(Char, A)],
      lineLength: Int,
      ruleRowSoFar: Boolean,
      backticks: Int
  ):

    def next(tagged: (Char, A), verbatim: A => Boolean): Education[A] =
      val (char, tag) = tagged
      if char == '\n' then Education(output :+ tagged, 0, ruleRowSoFar = false, backticks = 0)
      else
        val replacement =
          if verbatim(tag) || backticks % 2 == 1 then None else replacementFor(char, precedingWindow)
        val ticks = if char == '`' then backticks + 1 else backticks
        replacement match
          case Some((replaced, text)) =>
            Education(
              output.dropRight(replaced) ++ text.map(_ -> tag),
              lineLength - replaced + text.length,
              false,
              ticks
            )
          case None =>
            val ruleRow = (lineLength == 0 || ruleRowSoFar) && RuleRowCharacters.contains(char)
            Education(output :+ tagged, lineLength + 1, ruleRow, ticks)

    /** Stands in for the line so far: its last two characters, behind a non-rule character when the line is not a rule
      * row so that [[isRuleRowSoFar]] still answers for the whole line.
      */
    private def precedingWindow: String =
      val tail = output.takeRight(lineLength.min(2)).map(_._1).mkString
      if lineLength == 0 || ruleRowSoFar then tail else s"x$tail"

  private object Education:
    def start[A]: Education[A] = Education(Vector.empty, 0, ruleRowSoFar = false, backticks = 0)

  /** Whether the caret sits inside a Markdown code span, judged by an odd number of backticks before it on its line. */
  def withinInlineCode(precedingLineText: String): Boolean =
    precedingLineText.count(_ == '`') % 2 == 1

  /** A line holding nothing but hyphens, pipes, colons and spaces so far is a Markdown thematic break, a front-matter
    * fence or a table delimiter row (`---`, `|---|`, `:--:`), whose hyphens must stay literal.
    */
  private def isRuleRowSoFar(precedingLineText: String): Boolean =
    precedingLineText.nonEmpty && precedingLineText.forall(RuleRowCharacters.contains)

  /** A quote opens rather than closes at the start of the line, after whitespace, or after an opening bracket/dash --
    * everywhere else (typically right after a letter) it closes, which also covers the common apostrophe case ("don't",
    * "'tis" aside).
    */
  private def opensQuote(precedingLineText: String): Boolean =
    precedingLineText.lastOption.forall(c => c.isWhitespace || isOpeningBracket(c))

  /** An opening quote straight before a digit was an apostrophe eliding a number (`'90s`), not a quotation (#1954). */
  private def opensElision(precedingLineText: String): Boolean =
    precedingLineText.endsWith(LeftSingleQuote) && opensQuote(precedingLineText.dropRight(1))

  private def isOpeningBracket(c: Char): Boolean =
    c == '(' || c == '[' || c == '{' || c == '—' || c == '–'
