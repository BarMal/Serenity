package com.serenity.io

import java.nio.file.Path
import java.util.regex.Pattern

import scala.annotation.tailrec

/** The rules of one ignore file (`.gitignore`, `.ignore`, `info/exclude`, a global excludes file), in file order and
  * matched against paths relative to the directory holding the file. Follows gitignore(5): blank lines and `#` comments
  * are skipped, `!` negates, a trailing `/` matches directories only, a pattern with a `/` other than a trailing one is
  * anchored to the file's directory, `*` `?` `[...]` never cross a `/`, and `**` crosses any number of directories.
  */
final case class GitIgnore(rules: Vector[GitIgnore.Rule]):

  def isEmpty: Boolean = rules.isEmpty

  /** `Some(true)` when the last matching rule ignores `path`, `Some(false)` when it re-includes it, `None` when no rule
    * matches. `path` is `/`-separated.
    */
  def verdict(path: String, isDirectory: Boolean): Option[Boolean] =
    rules.findLast(_.matches(path, isDirectory)).map(!_.negated)

object GitIgnore:

  val empty: GitIgnore = GitIgnore(Vector.empty)

  final case class Rule(regex: String, negated: Boolean, directoriesOnly: Boolean, anchored: Boolean):
    private lazy val compiled: Pattern = Pattern.compile(regex)

    def matches(path: String, isDirectory: Boolean): Boolean =
      (isDirectory || !directoriesOnly) && compiled.matcher(if anchored then path else baseName(path)).matches()

  def parse(text: String): GitIgnore =
    GitIgnore(text.linesIterator.flatMap(parseLine).toVector)

  /** The `core.excludesFile` a git config sets, with a leading `~/` expanded against `home`. */
  def excludesFileFrom(gitConfig: String, home: Path): Option[Path] =
    gitConfig.linesIterator
      .map(_.trim)
      .collectFirst { case ExcludesFileSetting(value) => value.trim }
      .filter(_.nonEmpty)
      .map(value => if value.startsWith("~/") then home.resolve(value.drop(2)) else Path.of(value))

  private val ExcludesFileSetting = """(?i)excludesfile\s*=\s*(.*)""".r

  private def baseName(path: String): String = path.substring(path.lastIndexOf('/') + 1)

  private def parseLine(raw: String): Option[Rule] =
    val line = withoutTrailingSpaces(raw)
    if line.isEmpty || line.startsWith("#") then None
    else
      val negated         = line.startsWith("!")
      val pattern         = if negated then line.drop(1) else line
      val directoriesOnly = pattern.endsWith("/")
      val body            = if directoriesOnly then pattern.dropRight(1) else pattern
      Option.when(body.stripPrefix("/").nonEmpty)(
        Rule(globToRegex(body.stripPrefix("/")), negated, directoriesOnly, anchored = body.contains("/"))
      )

  // An unescaped trailing space is not part of the pattern; a backslash before the run keeps its first space.
  private def withoutTrailingSpaces(line: String): String =
    val trimmed = line.take(line.lastIndexWhere(_ != ' ') + 1)
    if trimmed.endsWith("\\") && trimmed.length < line.length then trimmed + " " else trimmed

  private def globToRegex(glob: String): String =
    @tailrec def loop(index: Int, out: StringBuilder): String =
      if index >= glob.length then out.toString
      else
        glob.charAt(index) match
          case '*' if glob.startsWith("**", index) =>
            val atSegmentStart = index == 0 || glob.charAt(index - 1) == '/'
            val next           = index + 2
            if atSegmentStart && glob.startsWith("/", next) then loop(next + 1, out.append("(?:.*/)?"))
            else if atSegmentStart && next == glob.length then loop(next, out.append(".*"))
            else loop(next, out.append("[^/]*"))
          case '*'                             => loop(index + 1, out.append("[^/]*"))
          case '?'                             => loop(index + 1, out.append("[^/]"))
          case '\\' if index + 1 < glob.length => loop(index + 2, out.append(literal(glob.charAt(index + 1))))
          case '[' =>
            characterClass(glob, index) match
              case Some((regex, next)) => loop(next, out.append(regex))
              case None                => loop(index + 1, out.append(literal('[')))
          case other => loop(index + 1, out.append(literal(other)))
    loop(0, new StringBuilder)

  private def literal(char: Char): String = Pattern.quote(char.toString)

  /** The regex for the bracket expression opening at `open` and the index after it; none when it is never closed. */
  private def characterClass(glob: String, open: Int): Option[(String, Int)] =
    val negated = glob.startsWith("!", open + 1) || glob.startsWith("^", open + 1)
    val start   = if negated then open + 2 else open + 1
    val close   = glob.indexOf(']', start + 1)
    Option.when(close >= 0) {
      val members = glob.substring(start, close).flatMap {
        case special @ ('\\' | '[' | ']' | '&' | '^') => s"\\$special"
        case other                                    => other.toString
      }
      ((if negated then "[^" else "[") + members + "]", close + 1)
    }

/** The ignore files in force at some directory of a project, as layers ordered from lowest to highest precedence. Each
  * layer's rules are relative to `base`, its directory relative to the project root (`""` for the root itself), so a
  * nested `.gitignore` overrides its parents' for paths below it and never applies outside it.
  */
final case class IgnoreRules(layers: Vector[IgnoreRules.Layer]):

  def withLayer(base: String, rules: GitIgnore): IgnoreRules =
    if rules.isEmpty then this else copy(layers = layers :+ IgnoreRules.Layer(base, rules))

  /** Whether `path` (project-relative, `/`-separated) is ignored; the highest layer with an opinion decides. */
  def ignores(path: String, isDirectory: Boolean): Boolean =
    val decisive =
      layers.reverseIterator.map(_.verdict(path, isDirectory)).collectFirst { case Some(ignored) => ignored }
    decisive.contains(true)

object IgnoreRules:

  val empty: IgnoreRules = IgnoreRules(Vector.empty)

  final case class Layer(base: String, rules: GitIgnore):

    def verdict(path: String, isDirectory: Boolean): Option[Boolean] =
      if base.isEmpty then rules.verdict(path, isDirectory)
      else if path.startsWith(base + "/") then rules.verdict(path.drop(base.length + 1), isDirectory)
      else None
