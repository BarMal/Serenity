package com.serenity.ui.openings

/** Which paragraphs between a chapter title and its opening are epigraphs, and so are skipped. */
enum EpigraphRule(val configKey: String):
  /** A Markdown `>` blockquote, or a rich-text paragraph that is centred, right-aligned or entirely italic. */
  case AutoDetect extends EpigraphRule("auto")

  /** Nothing is skipped: the first paragraph after the title is the candidate. */
  case Never extends EpigraphRule("never")

  /** Only paragraphs the caller marks as epigraphs; see the `explicitEpigraph` predicate of [[ChapterOpenings]]. */
  case ExplicitOnly extends EpigraphRule("marker")

object EpigraphRule:
  def fromConfigKey(key: String): Option[EpigraphRule] = values.find(_.configKey == key)

/** What a scene break, a line such as `***`, does to the paragraph after it. */
enum SceneBreakRule(val configKey: String):
  case NoOpening     extends SceneBreakRule("none")
  case SmallCapsOnly extends SceneBreakRule("small-caps")
  case FullOpening   extends SceneBreakRule("full")

object SceneBreakRule:
  def fromConfigKey(key: String): Option[SceneBreakRule] = values.find(_.configKey == key)

/** What happens when an opening is already a hand-made drop cap. Detection only reports it; the resolver applies this.
  */
enum HandDropCapRule(val configKey: String):
  case HandWins          extends HandDropCapRule("hand")
  case HandPlusSmallCaps extends HandDropCapRule("hand-small-caps")
  case OpeningWins       extends HandDropCapRule("opening")

object HandDropCapRule:
  def fromConfigKey(key: String): Option[HandDropCapRule] = values.find(_.configKey == key)

final case class OpeningRules(
    epigraph: EpigraphRule = EpigraphRule.AutoDetect,
    sceneBreak: SceneBreakRule = SceneBreakRule.NoOpening,
    handDropCap: HandDropCapRule = HandDropCapRule.HandWins
)

object OpeningRules:
  val default: OpeningRules = OpeningRules()

enum OpeningKind:
  case Chapter
  case SceneBreakSmallCaps
  case SceneBreakFull

/** The paragraph or line `line` opens a chapter or follows a scene break. `handDropCap` is set when the paragraph
  * already carries a hand-made [[com.serenity.richtext.ParagraphRole.DropCap]].
  */
final case class Opening(line: Int, kind: OpeningKind, handDropCap: Boolean)

object SceneBreaks:

  private val singles: Set[String] = Set("#", "⁂", "❧", "☙", "❦")

  private val repeatable: Set[Char] = Set('*', '-', '_', '~', '•', '·', '◆', '◇', '✦', '✧', '—')

  /** A lone ornament, or at least three of the same rule character, spaces ignored: `***`, `* * *`, `---`, `___`. */
  def isSceneBreak(text: String): Boolean =
    val compact = text.filterNot(char => char.isWhitespace || char == ' ')
    compact.nonEmpty && (singles.contains(compact) ||
      (compact.length >= 3 && repeatable.contains(compact.head) && compact.forall(_ == compact.head)))
