package com.serenity.command

import java.util.Locale

/** Search-matching and ranking helpers for the palette and settings search. Split out of `CommandRunner`'s companion
  * object to keep both under the architecture size targets -- see `CommandRunner`'s doc.
  */
private[command] object CommandRunnerSearch:

  private[command] val MaximumSettingSearchResults = 10

  private[command] def normalizedSearchTerm(term: String): String =
    term.trim
      .stripPrefix("\"")
      .stripSuffix("\"")
      .toLowerCase(Locale.ROOT)
      .replaceAll("[^\\p{L}\\p{N}]+", " ")
      .trim

  private[command] def isSpecificSettingQuery(term: String): Boolean =
    term.split(" ").count(_.nonEmpty) > 1

  private[command] def isExactSettingsTarget(item: CommandSurfaceItem, term: String): Boolean =
    item match
      case item: CommandSurfaceItem.SettingSearchItem =>
        val label = normalizedSearchTerm(item.label)
        val id    = normalizedSearchTerm(item.targetItemId)
        label == term || id == term
      case item: CommandSurfaceItem.GroupItem =>
        val label = normalizedSearchTerm(item.label)
        val id    = normalizedSearchTerm(item.id)
        label == term || id == term
      case _ => false

  private[command] def settingSearchRank(item: CommandSurfaceItem, breadcrumb: String, term: String): Option[Int] =
    val label         = normalizedSearchTerm(itemLabel(item))
    val id            = normalizedSearchTerm(item.id)
    val scope         = normalizedSearchTerm(breadcrumb)
    val terms         = term.split(" ").filter(_.nonEmpty).toList
    val allTermsMatch = terms.nonEmpty && terms.forall(token => s"$label $id $scope".contains(token))
    if label == term || id == term then Some(0)
    else if label.startsWith(term) || id.startsWith(term) then Some(1)
    else if allTermsMatch then Some(2)
    else None

  private[command] def itemLabel(item: CommandSurfaceItem): String =
    item match
      case CommandSurfaceItem.CommandItem(command)    => command.label
      case item: CommandSurfaceItem.OptionItem        => item.label
      case item: CommandSurfaceItem.InputItem         => item.label
      case item: CommandSurfaceItem.SettingSearchItem => item.label
      case item: CommandSurfaceItem.GroupItem         => item.label

  private[command] def itemHint(item: CommandSurfaceItem): Option[String] =
    item match
      case item: CommandSurfaceItem.OptionItem        => item.hint
      case item: CommandSurfaceItem.InputItem         => Some(item.hint)
      case item: CommandSurfaceItem.SettingSearchItem => item.hint
      case item: CommandSurfaceItem.GroupItem         => item.hint
      case _: CommandSurfaceItem.CommandItem          => None

  private[command] def itemEffectiveValue(item: CommandSurfaceItem): Option[String] =
    item match
      case item: CommandSurfaceItem.OptionItem => Some(item.selectedOption)
      case item: CommandSurfaceItem.InputItem  => Some(item.currentValue)
      case _                                   => None

  /** issue #1048: fuzzy score of `term` against one target string, or `None` when it doesn't match at all -- the same
    * shape VS Code/IntelliJ/Raycast-style fuzzy filters use. Case-insensitive. Checked in order, each tier strictly
    * outscoring every tier below it:
    *   1. exact match
    *   2. prefix match
    *   3. contiguous substring match anywhere (earlier, and starting right at a word boundary, scores higher -- "line"
    *      against "toggle-line-numbers" matches here, right after the "-" separator)
    *   4. scattered in-order subsequence match (every character of `term` appears in `target` in order, but not
    *      necessarily touching) -- always the lowest tier, so a real substring match never loses to a scattered one
    * A multi-word `term` is additionally tried as a per-token AND match (tiers 1-4 run independently for each
    * whitespace-separated token, all of which must match somewhere in `target`) and the better of the two wins -- this
    * is what lets a reordered or scattered query like "numbers line" still match "Toggle Line Numbers" even though it
    * is not, as a whole string, an in-order subsequence of the target. This restores the old per-token AND matcher's
    * coverage for multi-word queries without giving up the single whole-string match's scoring. This is the one matcher
    * every search call site in this cluster shares -- `CommandSearcher` (the palette) and `isStrongCommandMatch` below
    * both call this rather than keeping their own separate prefix/contains ladders.
    */
  private[command] def fuzzyScore(term: String, target: String): Option[Double] =
    val needle   = term.trim.toLowerCase(Locale.ROOT)
    val haystack = target.toLowerCase(Locale.ROOT)
    if needle.isEmpty then Some(0.0)
    else
      val wholeQueryScore = singleTermScore(needle, haystack)
      val tokenAndScore   = tokenAndFallbackScore(needle, haystack)
      (wholeQueryScore, tokenAndScore) match
        case (Some(whole), Some(tokenAnd)) => Some(math.max(whole, tokenAnd))
        case (Some(whole), None)           => Some(whole)
        case (None, fallback)              => fallback

  /** Tiers 1-4 of `fuzzyScore` against a single, already-normalized needle (either the whole query, or one token of it
    * in the AND fallback below).
    */
  private def singleTermScore(needle: String, haystack: String): Option[Double] =
    if haystack == needle then Some(1000.0)
    else if haystack.startsWith(needle) then Some(900.0)
    else
      val substringIndex = haystack.indexOf(needle)
      if substringIndex >= 0 then
        val wordBoundary = substringIndex == 0 || !haystack(substringIndex - 1).isLetterOrDigit
        Some(700.0 + (if wordBoundary then 100.0 else 0.0) - substringIndex.toDouble)
      else subsequenceScore(needle, haystack)

  /** issue #1048 regression fix: the whole-query sequence match above requires every character of `needle` to appear in
    * `target` in query order, so a reordered or scattered multi-word query (words present, but not in the order typed)
    * fails it entirely -- the old per-token AND matcher this replaced did not have that limitation. Splits `needle` on
    * whitespace and requires every token to independently match somewhere in `haystack` (AND semantics); `None` if
    * there is only one token (no benefit over the whole-query match) or if any token fails to match at all. The
    * combined score is the mean of the per-token scores, so it can only win over the whole-query score when the latter
    * is `None` or comes from the weaker scattered-subsequence tier.
    */
  private def tokenAndFallbackScore(needle: String, haystack: String): Option[Double] =
    val tokens = needle.split("\\s+").filter(_.nonEmpty).toList
    if tokens.length < 2 then None
    else
      val perTokenScores = tokens.map(token => singleTermScore(token, haystack))
      if perTokenScores.forall(_.isDefined) then Some(perTokenScores.flatten.sum / tokens.length) else None

  /** Greedy scattered-subsequence fallback: every character of `needle` must appear in `haystack` in order (not
    * necessarily contiguous), rewarding an earlier start and any contiguous runs found along the way. Always scores
    * below every tier in `fuzzyScore` above it (all under 700), so it only ever wins when nothing better matched.
    */
  private def subsequenceScore(needle: String, haystack: String): Option[Double] =
    @scala.annotation.tailrec
    def loop(needleIndex: Int, haystackIndex: Int, runLength: Int, score: Double, firstMatch: Int): Option[Double] =
      if needleIndex >= needle.length then Some(200.0 + score - firstMatch.toDouble)
      else if haystackIndex >= haystack.length then None
      else if needle(needleIndex) == haystack(haystackIndex) then
        val nextRun = runLength + 1
        loop(
          needleIndex + 1,
          haystackIndex + 1,
          nextRun,
          score + 1.0 + nextRun.toDouble,
          if firstMatch < 0 then haystackIndex else firstMatch
        )
      else loop(needleIndex, haystackIndex + 1, 0, score, firstMatch)
    loop(0, 0, 0, 0.0, -1)

  private[command] def isStrongCommandMatch(command: Command, term: String): Boolean =
    // A "strong" match is at least a contiguous substring hit (fuzzyScore's tier 3, >= 700) -- a scattered
    // subsequence match exists but is too weak to promote a command above a settings-group breadcrumb match on its
    // own; the exact-match tier in `CommandRunner.visibleItems` already separates out tier 1/2 above this.
    List(command.name, command.label, command.description).flatMap(fuzzyScore(term, _)).exists(_ >= 700.0)

  private[command] def isExactCommandMatch(command: Command, term: String): Boolean =
    val normalizedTerm = normalizedSearchTerm(term)
    normalizedSearchTerm(command.name) == normalizedTerm ||
    normalizedSearchTerm(command.label) == normalizedTerm

  private[command] def directGroupSearchText(group: CommandSurfaceItem.GroupItem): String =
    normalizedSearchTerm(s"${group.id} ${group.label} ${group.hint.getOrElse("")}")

  private[command] def directItemSearchText(item: CommandSurfaceItem): String =
    item match
      case group: CommandSurfaceItem.GroupItem =>
        directGroupSearchText(group)
      case other =>
        normalizedSearchTerm(s"${other.id} ${other.searchText}")
