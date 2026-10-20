import scala.util.matching.Regex

/** Purity rules for `src/main`, run by [[ArchitectureChecks]] (#1677).
  *
  * Two rules, both on declarations rather than lines, so local mutation inside a method -- which
  * docs/coding-standards.md allows when contained -- is never flagged:
  *
  *   1. '''Mutable declarations.''' A `val`/`lazy val`/`var` that is a member of a top-level object (or of anything under
  *      `state/models`) may not hold a mutability token anywhere in its whole statement, continuation lines included.
  *      A `var` member of a top-level object is itself a violation.
  *   2. '''Object-held stateful instances.''' A class is stateful when a member of it holds a mutability token, is a
  *      `var`, or builds a `Ref.unsafe`. A top-level object's `val` whose type or right-hand side constructs one --
  *      `new C`, `C(...)`, or a factory declared to return one, such as `WrappedLineCache.bounded()` -- is a
  *      process-wide instance of mutable state.
  *
  * What a method does inside its own body, and what an `IO` constructor defers, is not part of the declaration.
  * Tokens match on word boundaries, so `immutable.` is not `mutable.`, and the `java.util` collections match only
  * where the file imports them (or writes them fully qualified), so Scala's own `HashMap` is never caught.
  */
object PurityChecks {

  import ArchitectureChecks.Violation

  /** A mutability token: a name for messages and the regex that finds it in code with comments and strings removed. */
  final case class Token(name: String, pattern: Regex)

  private def word(name: String): Token = Token(name, ("""\b""" + Regex.quote(name) + """\b""").r)

  /** The `java.util` collections that are mutable state; each matches only when the file imports or qualifies it. */
  val JavaUtilCollections: Seq[String] =
    Seq("HashMap", "LinkedHashMap", "ArrayDeque", "ArrayList", "WeakHashMap", "IdentityHashMap")

  val Tokens: Seq[Token] = Seq(
    word("AtomicReference"),
    word("AtomicInteger"),
    word("AtomicLong"),
    word("AtomicBoolean"),
    word("synchronized"),
    word("ThreadLocal"),
    word("ConcurrentHashMap"),
    Token("mutable.", """\bmutable\.""".r)
  )

  private val StatefulMarker: Regex = """\bRef\.unsafe\b""".r

  private val JavaUtilImport: Regex = """(?s)import\s+java\.util\.(\{[^}]*\}|[\w*]+)""".r

  /** The `java.util` collection names a file can write bare: imported by name, renamed, or via a wildcard. */
  def javaUtilNames(lines: Vector[String]): Set[String] =
    JavaUtilImport
      .findAllMatchIn(lines.mkString("\n"))
      .flatMap { m =>
        val selectors = m.group(1).stripPrefix("{").stripSuffix("}").split(',').map(_.trim).filter(_.nonEmpty)
        selectors.flatMap { selector =>
          val parts = selector.split("""\s*(=>|\bas\b)\s*""").map(_.trim)
          if (parts.headOption.exists(p => p == "*" || p == "_")) JavaUtilCollections
          else if (parts.headOption.exists(JavaUtilCollections.contains)) Seq(parts.last)
          else Seq.empty[String]
        }
      }
      .toSet

  private def javaUtilTokens(imported: Set[String]): Seq[Token] = {
    val qualified = Token(
      "java.util collection",
      ("""\bjava\.util\.(?:""" + JavaUtilCollections.mkString("|") + """)\b""").r
    )
    if (imported.isEmpty) Seq(qualified)
    else
      Seq(
        qualified,
        Token("java.util collection", ("""(?<![\w.])(?:""" + imported.map(Regex.quote).mkString("|") + """)\b""").r)
      )
  }

  private val VarMember = Token("var", """\bvar\b""".r)

  private val DeclarationModifier =
    """(?:override|private(?:\[[^\]]+\])?|protected(?:\[[^\]]+\])?|final|implicit|inline|transparent|lazy|open|abstract|sealed)"""

  private val Declaration: Regex =
    ("""^(\s*)(?:@\w+(?:\([^)]*\))?\s+)*(?:""" + DeclarationModifier + """\s+)*(val|var|def|given)\b\s*([\w`]+|\([^)]*\))?""").r

  private val TypeStart: Regex =
    """^(\s*)(?:(?:override|private(?:\[[^\]]+\])?|protected(?:\[[^\]]+\])?|final|sealed|abstract|implicit|case|open)\s+)*(object|class|trait|enum)\s+(\w+)""".r

  private val LocalDeclaration: Regex =
    ("""^\s*(?:""" + DeclarationModifier + """\s+)*(?:val|var|def|given|object|class|trait|enum|type)\b""").r

  /** The first `IO`/`IO.delay`/`IO.blocking`... call, after which the statement is a deferred effect, not a value. */
  private val IoConstructor: Regex = """\bIO(?:\.\w+)?\s*[({]""".r

  private val StringLiteral: Regex = "\"(?:[^\"\\\\]|\\\\.)*\"".r

  final private case class CodeLine(index: Int, indent: Int, text: String)

  /** The non-blank lines with comments and string contents removed, each with its indentation. */
  private def codeLines(lines: Vector[String]): Vector[CodeLine] = {
    var inBlock = false
    lines.zipWithIndex.flatMap { case (line, index) =>
      val trimmed = line.trim
      if (inBlock) {
        if (trimmed.contains("*/")) inBlock = false
        None
      } else if (trimmed.isEmpty || trimmed.startsWith("//") || trimmed.startsWith("*")) None
      else if (trimmed.startsWith("/*")) {
        if (!trimmed.contains("*/")) inBlock = true
        None
      } else {
        val withoutComment = line.indexOf(" // ") match {
          case -1 => line
          case at => line.substring(0, at)
        }
        Some(CodeLine(index, line.indexWhere(!_.isWhitespace), StringLiteral.replaceAllIn(withoutComment, "\"\"")))
      }
    }
  }

  /** A `val`/`lazy val`/`var` member (its `statement` is the whole declaration) or a member-level line that is not one. */
  final case class Member(owner: String, name: String, keyword: String, statement: String, inObjectChain: Boolean)

  final case class ClassInfo(name: String, stateful: Boolean)

  final case class Factory(owner: String, method: String, returns: String)

  final case class Parsed(
      path: String,
      members: Seq[Member],
      classes: Seq[ClassInfo],
      factories: Seq[Factory],
      inModels: Boolean,
      javaUtil: Set[String]
  )

  final private case class Frame(name: String, isObject: Boolean, isType: Boolean, indent: Int)

  /** Cut at the first deferred `IO` constructor, so what an effect does when run is not part of the declaration. */
  private def beforeEffect(text: String): String =
    IoConstructor.findFirstMatchIn(text).fold(text)(m => text.substring(0, m.start))

  private def balanced(header: String): String = {
    val kept = new StringBuilder
    var depth = 0
    header.foreach { c =>
      if (c == '(' || c == '[') { if (depth == 0) kept.append(c); depth += 1 }
      else if (c == ')' || c == ']') { depth -= 1; if (depth == 0) kept.append(c) }
      else if (depth == 0) kept.append(c)
    }
    kept.toString
  }

  private val ReturnType: Regex = """\bdef\s+(\w+)\s*(?:[(\[][)\]]\s*)*:\s*([\w.]+)""".r

  /** The declared return type of a `def`, read from its header with parameter lists emptied. */
  def returnType(header: String): Option[(String, String)] =
    ReturnType.findFirstMatchIn(balanced(header)).map(m => m.group(1) -> m.group(2).split('.').last)

  def parse(path: String, lines: Vector[String]): Parsed = {
    val code      = codeLines(lines)
    val inModels  = path.contains("com/serenity/state/models")
    var stack     = List.empty[Frame]
    val members   = Vector.newBuilder[Member]
    val classes   = Vector.newBuilder[ClassInfo]
    val factories = Vector.newBuilder[Factory]
    var statefulClasses = Set.empty[String]
    var allClasses      = Vector.empty[String]

    def markStateful(): Unit =
      stack.find(_.isType).filterNot(_.isObject).foreach(frame => statefulClasses += frame.name)

    def statementAt(position: Int): (String, Int) = {
      val head = code(position)
      var end  = position + 1
      val text = new StringBuilder(head.text)
      var skipUntil = -1
      while (end < code.length && code(end).indent > head.indent) {
        val next = code(end)
        if (skipUntil >= 0 && next.indent > skipUntil) ()
        else {
          skipUntil = -1
          if (LocalDeclaration.findPrefixOf(next.text).isDefined) skipUntil = next.indent
          else text.append('\n').append(next.text)
        }
        end += 1
      }
      (text.toString, end)
    }

    var position = 0
    while (position < code.length) {
      val line = code(position)
      val closesHeader = line.text.trim.startsWith(")")
      stack = stack.dropWhile(frame => if (closesHeader) frame.indent > line.indent else frame.indent >= line.indent)
      val insideBody = stack.headOption.exists(!_.isType)
      val typeStart  = TypeStart.findPrefixMatchOf(line.text)
      val chain      = stack.reverse.map(_.name).mkString(".")
      if (insideBody) position += 1
      else if (typeStart.isDefined) {
        val kind = typeStart.map(_.group(2)).getOrElse("")
        val name = typeStart.map(_.group(3)).getOrElse("")
        // a header's closing line (`):`) must not pop the type it closes, so a multi-line constructor keeps its owner
        if (kind != "object") allClasses :+= name
        stack = Frame(name, kind == "object", isType = true, line.indent) :: stack
        position += 1
      } else
        Declaration.findPrefixMatchOf(line.text) match {
          case Some(declared) =>
            val keyword = declared.group(2)
            val name    = Option(declared.group(3)).getOrElse("_")
            val (statement, end) = statementAt(position)
            val owner = if (chain.isEmpty) "<file>" else chain
            val inObjects = stack.nonEmpty && stack.forall(_.isObject)
            if (keyword == "def") {
              if (inObjects) {
                val header = statement.linesIterator.takeWhile(!_.trim.startsWith("=")).mkString(" ")
                returnType(header).foreach { case (method, returns) => factories += Factory(chain, method, returns) }
              }
            } else {
              val isLazy = line.text.contains("lazy ")
              val kept   = beforeEffect(statement)
              members += Member(owner, name, if (isLazy) "lazy val" else keyword, kept, inObjects)
              if (stack.exists(_.isType) && !inObjects) {
                if (keyword == "var" || Tokens.exists(_.pattern.findFirstIn(kept).isDefined) ||
                  StatefulMarker.findFirstIn(kept).isDefined) markStateful()
              }
            }
            stack = Frame(name, isObject = false, isType = false, line.indent) :: stack
            position = end
          case None =>
            val text       = line.text.trim
            val memberLine = !text.startsWith("import ") && !text.startsWith("package ")
            if (memberLine && stack.exists(_.isType)) {
              val inObjects = stack.forall(_.isObject)
              val flagged =
                Tokens.exists(_.pattern.findFirstIn(text).isDefined) || StatefulMarker.findFirstIn(text).isDefined
              if (flagged && !inObjects) markStateful()
              if (flagged && inModels)
                members += Member(chain, s"line ${line.index + 1}", "param", beforeEffect(text), inObjects)
            }
            position += 1
        }
    }
    allClasses.distinct.foreach(name => classes += ClassInfo(name, statefulClasses.contains(name)))
    Parsed(path, members.result(), classes.result(), factories.result(), inModels, javaUtilNames(lines))
  }

  private def tokensIn(parsed: Parsed, text: String): Seq[String] =
    (Tokens ++ javaUtilTokens(parsed.javaUtil)).filter(_.pattern.findFirstIn(text).isDefined).map(_.name).distinct

  /** Rule 1. Declarations are named, not numbered, so the baseline survives edits elsewhere in the file. */
  def mutableDeclarations(parsed: Parsed): Seq[Violation] =
    if (!parsed.path.startsWith("main/")) Nil
    else
      parsed.members.flatMap { member =>
        val tokens = tokensIn(parsed, member.statement) ++
          (if (member.keyword == "var" && member.inObjectChain) Seq(VarMember.name) else Nil)
        if (tokens.isEmpty || !(parsed.inModels || member.inObjectChain)) Nil
        else {
          val where = if (parsed.inModels) "a state model" else "a top-level object"
          Seq(
            Violation(
              parsed.path,
              s"mutable declaration `${member.owner}.${member.name}` holds ${tokens.mkString(", ")}: $where may not " +
                "own mutable state -- scope it to an owning instance instead (#1677)",
              1
            )
          )
        }
      }

  /** Everything a stateful class can be called by in a declaration: itself, and the factories declared to return it. */
  final case class StatefulIndex(classes: Set[String], factories: Seq[Factory])

  def index(all: Seq[Parsed]): StatefulIndex = {
    val stateful = all.filter(_.path.startsWith("main/")).flatMap(_.classes).filter(_.stateful).map(_.name).toSet
    StatefulIndex(stateful, all.filter(_.path.startsWith("main/")).flatMap(_.factories).filter(f => stateful(f.returns)))
  }

  private val FirstAssignment: Regex = """(?<![=!<>])=(?![=>])""".r

  private def mentions(cls: String, text: String): Boolean =
    ("""(?<![\w.])""" + Regex.quote(cls) + """\b""").r.findFirstIn(text).isDefined

  private def builds(cls: String, rhs: String): Boolean = {
    val quoted = Regex.quote(cls)
    ("""\bnew\s+(?:\w+\.)*""" + quoted + """\b""").r.findFirstIn(rhs).isDefined ||
    ("""(?<!\w)(?:\w+\.)*""" + quoted + """\s*(?:\[[^\]]*\])?\s*\(""").r.findFirstIn(rhs).isDefined
  }

  private def callsFactory(factory: Factory, member: Member, rhs: String): Boolean = {
    val owner  = factory.owner.split('.').last
    val method = Regex.quote(factory.method)
    ("""\b""" + Regex.quote(owner) + """\.""" + method + """\b""").r.findFirstIn(rhs).isDefined ||
    (member.owner.split('.').contains(owner) &&
      ("""(?<![\w.])""" + method + """\s*(?:\[[^\]]*\])?\s*\(""").r.findFirstIn(rhs).isDefined)
  }

  /** The stateful class a declaration's type or right-hand side names, if any. */
  private def constructed(index: StatefulIndex, member: Member): Option[String] = {
    val statement = member.statement
    val split     = FirstAssignment.findFirstMatchIn(statement).map(_.start).getOrElse(statement.length)
    val declared  = statement.substring(0, split).replaceFirst("""^[^:]*""", "")
    val rhs       = statement.substring(split)
    index.classes.toSeq.sorted
      .find(cls => mentions(cls, declared) || builds(cls, rhs))
      .orElse(index.factories.find(callsFactory(_, member, rhs)).map(_.returns))
  }

  /** Rule 2. */
  def statefulInstances(parsed: Parsed, index: StatefulIndex): Seq[Violation] =
    if (!parsed.path.startsWith("main/")) Nil
    else
      parsed.members
        .filter(m => m.inObjectChain && m.keyword != "param")
        .flatMap { member =>
          constructed(index, member).map { cls =>
            Violation(
              parsed.path,
              s"object-held stateful instance `${member.owner}.${member.name}` is a $cls: a top-level object may not " +
                "own a mutable instance -- have the runtime that uses it own it (#1677)",
              1
            )
          }
        }

  /** A declaration that must stay object-held, with the reason it cannot move. Not part of the shrinking baseline: an
    * entry is removed only by a redesign (docs/allowlist-review.md), and one that no longer matches fails the check.
    */
  final case class Allowed(path: String, declaration: String, reason: String)

  def allowlisted(allowed: Seq[Allowed], violation: Violation): Boolean =
    allowed.exists(a => violation.path == a.path && violation.detail.contains(s"`${a.declaration}`"))

  def staleAllowlist(allowed: Seq[Allowed], raw: Seq[Violation]): Seq[Allowed] =
    allowed.filterNot(a => raw.exists(v => allowlisted(Seq(a), v)))

  /** Regression guard for the rules themselves, run from [[ArchitectureChecks.selfTest]]. */
  def selfTest(): Unit = {
    val main = "main/scala/com/serenity/ui/layout/Sample.scala"

    def declarations(lines: Vector[String], path: String = main): Seq[Violation] =
      mutableDeclarations(parse(path, lines))

    def instances(lines: Vector[String], others: Vector[String]*): Seq[Violation] = {
      val files = (lines +: others).zipWithIndex.map { case (source, i) => parse(s"main/scala/com/serenity/s/F$i.scala", source) }
      statefulInstances(files.head, index(files))
    }

    def expect(description: String, found: Seq[Violation], caught: Boolean): Unit =
      require(
        found.nonEmpty == caught,
        s"PurityChecks self-test failed ($description): expected the declaration to be " +
          s"${if (caught) "caught" else "ignored"}, got ${found.size} violation(s): ${found.map(_.detail).mkString("; ")}"
      )

    def inObject(member: String, imports: String*): Vector[String] =
      (imports.toVector :+ "object Sample:") ++ member.split("\n").map("  " + _)

    // Every token, as a field of a top-level object.
    Seq(
      "AtomicReference" -> "private val x = new java.util.concurrent.atomic.AtomicReference[Int](0)",
      "AtomicInteger"   -> "private val x = new AtomicInteger(0)",
      "AtomicLong"      -> "private val x = AtomicLong(0)",
      "AtomicBoolean"   -> "private val x = new AtomicBoolean(false)",
      "ThreadLocal"     -> "private val x = ThreadLocal.withInitial(() => 1)",
      "ConcurrentHashMap" -> "private val x = ConcurrentHashMap[String, Int]()",
      "synchronized"    -> "private val x = new Object().synchronized { 1 }",
      "mutable."        -> "private val x = scala.collection.mutable.Map.empty[Int, Int]"
    ).foreach { case (token, member) =>
      expect(s"$token held by a top-level object", declarations(inObject(member)), caught = true)
    }

    // The java.util collections: fully qualified, imported by name, renamed, imported with a wildcard, imported by a
    // list wrapped over several lines.
    JavaUtilCollections.foreach { name =>
      expect(s"java.util.$name written fully qualified", declarations(inObject(s"private val x = new java.util.$name[Int, Int]()")), caught = true)
      expect(s"java.util.$name imported by name", declarations(inObject(s"private val x = new $name[Int, Int]()", s"import java.util.$name")), caught = true)
      expect(
        s"java.util.$name imported from a list",
        declarations(inObject(s"private val x = $name[Int, Int]()", s"import java.util.{ArrayList, $name}")),
        caught = true
      )
      expect(
        s"java.util.$name renamed on import",
        declarations(inObject(s"private val x = new Mine[Int, Int]()", s"import java.util.{$name => Mine}")),
        caught = true
      )
      expect(
        s"java.util.$name imported by a wildcard",
        declarations(inObject(s"private val x = new $name[Int, Int]()", "import java.util.*")),
        caught = true
      )
      expect(
        s"java.util.$name imported by a list wrapped over lines",
        declarations(inObject(s"private val x = new $name[Int, Int]()", "import java.util.{", s"  Date,", s"  $name", "}")),
        caught = true
      )
      expect(
        s"a Scala $name with no java.util import",
        declarations(inObject(s"private val x = $name(1 -> 2)", s"import scala.collection.immutable.$name")),
        caught = false
      )
    }
    expect("immutable. is not mutable.", declarations(inObject("private val x = scala.collection.immutable.Map(1 -> 2)")), caught = false)
    expect("immutable.HashMap is not a java.util one", declarations(inObject("private val x = immutable.HashMap(1 -> 2)", "import java.util.HashMap")), caught = false)
    expect("a word that merely contains a token", declarations(inObject("private val x = DoesNotSynchronizedAtAll")), caught = false)

    // Declaration level: the whole statement counts, a method's own body does not.
    expect(
      "a token on a continuation line of the declaration",
      declarations(inObject("private val x = Foo(\n  name = \"x\",\n  cache = ConcurrentHashMap[Int, Int]()\n)")),
      caught = true
    )
    expect(
      "a mutable collection local to a method of an object",
      declarations(inObject("def sum(xs: Seq[Int]): Int =\n  val buffer = scala.collection.mutable.ArrayBuffer(xs*)\n  buffer.sum")),
      caught = false
    )
    expect(
      "a ConcurrentHashMap and an AtomicLong local to a method of an object",
      declarations(inObject("def f(): Int =\n  val m = new ConcurrentHashMap[Int, Int]()\n  val n = AtomicLong(0)\n  m.size")),
      caught = false
    )
    expect(
      "a synchronized block in a method of an object",
      declarations(inObject("private val lock = new Object\ndef touch(): Unit = lock.synchronized { () }")),
      caught = false
    )
    expect(
      "a local declared inside the right-hand side of a value",
      declarations(inObject("val total: Int =\n  val buffer = scala.collection.mutable.ArrayBuffer(1)\n  buffer.sum")),
      caught = false
    )
    expect(
      "mutable state created inside an IO a value defers",
      declarations(inObject("val run: IO[Unit] = IO {\n  val m = new ConcurrentHashMap[Int, Int]()\n  m.put(1, 2)\n  ()\n}")),
      caught = false
    )
    expect(
      "mutable state created in IO.delay on one line",
      declarations(inObject("val run: IO[Int] = IO.delay(scala.collection.mutable.Map(1 -> 2).size)")),
      caught = false
    )
    expect(
      "a field of a class nested in an object",
      declarations(inObject("final private class Cache:\n  private val ref = new AtomicReference[Int](0)")),
      caught = false
    )
    expect(
      "a comment that names a token",
      declarations(inObject("// like an AtomicReference-based loop\nprivate val x = 1")),
      caught = false
    )
    expect(
      "a string that names a token",
      declarations(inObject("private val message = \"never use mutable. state\"")),
      caught = false
    )
    expect(
      "test sources are not scanned",
      declarations(inObject("private val x = new AtomicInteger(0)"), path = "test/scala/com/serenity/SampleSpec.scala"),
      caught = false
    )

    // var in a top-level object.
    expect("a var held by a top-level object", declarations(inObject("private var counter = 0")), caught = true)
    expect("a var local to a method of an object", declarations(inObject("def f(): Int =\n  var n = 0\n  n += 1\n  n")), caught = false)
    expect(
      "a var field of a class",
      declarations(Vector("final class Sample:", "  private var counter = 0")),
      caught = false
    )

    // state/models holds none of it, in a class or an object.
    val models = "main/scala/com/serenity/state/models/Sample.scala"
    expect(
      "a token as a case class parameter in state/models",
      declarations(Vector("final case class Sample(", "  private val cache: AtomicReference[Int]", ")"), models),
      caught = true
    )
    expect(
      "a token as a plain case class parameter in state/models",
      declarations(Vector("final case class Sample(", "  cache: AtomicReference[Int]", ")"), models),
      caught = true
    )
    expect(
      "a mutable collection local to a method of a state model",
      declarations(Vector("final case class Sample(n: Int):", "  def total: Int =", "    val b = scala.collection.mutable.ArrayBuffer(n)", "    b.sum"), models),
      caught = false
    )

    // Object-held stateful instances.
    val statefulClass = Vector("final class Memo:", "  private val entries = new ConcurrentHashMap[Int, Int]()")
    expect("an object building a stateful class with new", instances(inObject("val shared = new Memo()"), statefulClass), caught = true)
    expect("an object building a stateful class through apply", instances(inObject("val shared = Memo[Int, Int](512)"), statefulClass), caught = true)
    expect("an object typed as a stateful class", instances(inObject("val shared: Memo = null"), statefulClass), caught = true)
    expect("a lazy val building a stateful class", instances(inObject("lazy val shared = new Memo()"), statefulClass), caught = true)
    expect(
      "an object calling a factory declared to return a stateful class",
      instances(inObject("val cache = Caches.bounded()"), statefulClass, Vector("object Caches:", "  def bounded(max: Int = 3): Memo =", "    new Memo()")),
      caught = true
    )
    expect(
      "an object calling a factory of its own",
      instances(Vector("object Sample:", "  def fresh(): Memo = new Memo()", "  val cache = fresh()"), statefulClass),
      caught = true
    )
    expect(
      "a class holding a stateful class (instance-owned)",
      instances(Vector("final class Owner:", "  private val memo = new Memo()"), statefulClass),
      caught = false
    )
    expect("a stateless class built by an object", instances(inObject("val shared = new Plain()"), Vector("final class Plain:", "  val n = 1")), caught = false)
    expect(
      "a class whose only mutability is local to a method",
      instances(inObject("val shared = new Local()"), Vector("final class Local:", "  def f(): Int =", "    val b = scala.collection.mutable.ArrayBuffer(1)", "    b.sum")),
      caught = false
    )
    expect(
      "a class holding a Ref.unsafe is stateful",
      instances(inObject("val shared = new Cell()"), Vector("final class Cell:", "  private val ref = Ref.unsafe[IO, Int](0)")),
      caught = true
    )
    expect(
      "a class with a var field is stateful",
      instances(inObject("val shared = new Counter()"), Vector("final class Counter:", "  private var n = 0")),
      caught = true
    )
    expect(
      "a class with a multi-line constructor holding a token is stateful",
      instances(inObject("val shared = new Wide()"), Vector("final class Wide(", "    name: String,", "    ref: AtomicReference[Int] = new AtomicReference(0)", "):", "  def f = 1")),
      caught = true
    )
    expect("an object value that is plain data", instances(inObject("val Default = Settings(1)"), statefulClass), caught = false)
    expect("a similarly named value", instances(inObject("val MemoSize = 4"), statefulClass), caught = false)

    // The allowlist matches a declaration by name and goes stale when the declaration goes.
    val allowed = Allowed(main, "Sample.x", "test")
    val hit     = declarations(inObject("private val x = new AtomicInteger(0)")).head
    require(allowlisted(Seq(allowed), hit), "PurityChecks self-test failed: an allowlisted declaration was not recognised")
    require(!allowlisted(Seq(allowed.copy(declaration = "Sample.y")), hit), "PurityChecks self-test failed: allowlist matched a different declaration")
    require(!allowlisted(Seq(allowed.copy(path = "main/Other.scala")), hit), "PurityChecks self-test failed: allowlist matched a different file")
    require(staleAllowlist(Seq(allowed), Nil) == Seq(allowed), "PurityChecks self-test failed: an unmatched allowlist entry was not reported")
    require(staleAllowlist(Seq(allowed), Seq(hit)).isEmpty, "PurityChecks self-test failed: a matched allowlist entry was reported stale")
  }
}
