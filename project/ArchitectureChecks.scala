import sbt.*

import scala.io.Source

/** Structural checks the compiler and WartRemover cannot express.
  *
  * Both checks are ratchets rather than fixed thresholds. A baseline file records every location that exceeds the
  * target today; the check fails when a file gets worse, when a new file starts out over target, or when a baseline
  * entry has been fixed but not removed. Existing debt therefore never blocks unrelated work, while the list can only
  * shrink.
  *
  * The number itself is the arbitrary part -- the ratchet is what makes it meaningful.
  */
object ArchitectureChecks {

  /** A method longer than this is doing more than one thing. Chosen to sit just above the 98.5% of methods already
    * under 40 lines, so it catches the tail without churning healthy code.
    */
  val MaxMethodLines = 80

  /** A backstop for file sprawl. Deliberately generous: the method check is the real signal. */
  val MaxFileLines = 600

  /** One forbidden-import rule: `pkg` gates which files it applies to (a substring of the relativised path),
    * `forbidden` is what a matching file may not reference, and `mainOnly` narrows that further to `src/main` --
    * for a rule whose target is reducer/model *production* purity, a `src/test` spec driving the same
    * effect-boundary helpers to simulate the full post-reduce pipeline in test setup is not the violation the
    * rule exists to catch (#1676).
    */
  final case class ImportRule(pkg: String, forbidden: Seq[String], reason: String, mainOnly: Boolean = false)

  /** Layering rules the package structure implies but nothing enforces.
    *
    * Reducers are pure state transitions. Reaching into AWT or the layout engine from one is what
    * docs/coding-standards.md forbids and what issue #862 exists to undo; this stops it coming back.
    */
  val ForbiddenImports: Seq[ImportRule] = Seq(
    ImportRule(
      "com/serenity/state/reducers",
      Seq(
        "java.awt",
        "com.serenity.ui.fonts",
        "com.serenity.ui.layout.LayoutEngine",
        "com.serenity.ui.renderer",
        "com.serenity.state.manager"
      ),
      "reducers must stay pure: take measured geometry as a parameter instead of reaching for AWT, the layout " +
        "engine or the effect-boundary manager -- a fully-qualified reference is still a reference (#1676)",
      mainOnly = true
    ),
    ImportRule(
      "com/serenity/state/reducers",
      Seq("cats.effect"),
      "reducers are pure transitions (#1697): emit an AppEffect for the shell to run instead of an IO"
    ),
    ImportRule(
      "com/serenity/state/models",
      Seq("java.awt.Graphics", "com.serenity.ui.renderer", "com.serenity.state.manager"),
      "state models describe data, not painting or effect-boundary orchestration",
      mainOnly = true
    ),
    // Issue #1669: the state layer and the command layer read `Runtime.capabilities`/`FrontendCapabilities`, never a
    // concrete frontend's own implementation package -- `com.serenity.ui.tui` (the TUI frontend: `TerminalShell`,
    // `TuiRuntime`, the terminal render surface) or `com.serenity.ui.terminal` (the GUI frontend's Swing window and
    // canvas -- there is no literal `ui.swing` package in this codebase, so this is that boundary's closest real
    // equivalent). `Main` is deliberately exempt: it is the one place a `Frontend` is selected and constructed, and
    // must reference both implementation packages to do so.
    ImportRule(
      "com/serenity/state",
      Seq("com.serenity.ui.tui", "com.serenity.ui.terminal"),
      "the state layer must stay frontend-agnostic (issue #1669): read Runtime.capabilities instead of reaching into " +
        "a concrete frontend's own implementation package",
      mainOnly = true
    ),
    // #1206: the manuscript model, compiler and writers are pure; reading sources, dialogs and the disk belong to the
    // shell, and the future paginator must not measure through AWT.
    ImportRule(
      "com/serenity/manuscript",
      Seq("java.awt", "org.apache.pdfbox", "org.apache.fontbox", "cats.effect", "com.serenity.state"),
      "the manuscript package is the pure export core: take sources and settings as values, and leave IO, AWT, " +
        "font parsing and the editor state to the shell",
      mainOnly = true
    ),
    // S5: the exporting package is the effectful edge of that core (font loading, later the PDF painter); it reads the
    // manuscript model but never the editor state, and never AWT, so measurement and painting stay on one font file.
    ImportRule(
      "com/serenity/exporting/",
      Seq("java.awt", "com.serenity.state"),
      "the exporting package measures and paints from the bundled font files through FontBox/PDFBox: no AWT, and the " +
        "editor state reaches it only as values",
      mainOnly = true
    ),
    ImportRule(
      "com/serenity/command",
      Seq("com.serenity.ui.tui", "com.serenity.ui.terminal"),
      "the command layer must stay frontend-agnostic (issue #1669): read FrontendCapabilities instead of reaching " +
        "into a concrete frontend's own implementation package",
      mainOnly = true
    )
  )

  /** #1911: the files that decide an event on the state dispatcher do no I/O. Disk work goes to a lane job in a file of
    * its own and comes back as an `EffectResult`, so a key never waits behind a read or a write.
    */
  val DispatcherPathFiles: Seq[String] = Seq(
    "main/scala/com/serenity/state/manager/StateManagerEventPipeline.scala",
    "main/scala/com/serenity/state/manager/EventPipelineTransitions.scala"
  )

  val DispatcherBlockingCalls: Seq[String] = Seq("IO.blocking", "IO.interruptible", "Files.", "FileUtils.")

  val DispatcherBlockingReason: String =
    "the dispatcher path does no I/O (#1911): hand disk work to a lane job and take its answer as an EffectResult"

  /** Blocking/synchronous escape hatches out of `IO` that #1434 removed from src/main. Test code legitimately
    * calls these to drive `IO` synchronously in specs, so this is scoped to `main/` only -- the same split
    * `Test / scalacOptions ~= (_.filterNot(...))` already draws in build.sbt for WartRemover.
    *
    * `.unsafeRunAndForget` is deliberately not listed: it is asynchronous (fire-and-forget via a `Dispatcher`,
    * as in AppRuntime and TerminalShell) rather than the blocking anti-pattern #1434 was about.
    */
  val ForbiddenCalls: Seq[(String, Seq[String], String)] = Seq(
    (
      "main/",
      Seq(".unsafeRunSync", ".unsafeRunTimed"),
      "runs an IO synchronously outside the Cats Effect runtime (#1434) -- use IOApp, a Resource/Dispatcher " +
        "boundary, or push the IO to the edge instead"
    )
  ) ++ DispatcherPathFiles.map(file => (file, DispatcherBlockingCalls, DispatcherBlockingReason))

  /** State ownership (#1697): only the dispatcher/ModelCommit layer holds the model `Ref`. Capabilities read state
    * through an `IO[AppState]` and write it only through `ModelCommit`'s validated commits, so a raw `Ref` -- or a
    * `Ref.lens` view of one -- anywhere else is a write path that can skip validation.
    *
    * `StateManager` allocates the model ref and `StateManagerRuntime` carries it to `StateManagerOperationBoundary`,
    * which owns the dispatcher and builds the one `ModelCommit` over it.
    */
  val StateOwnership: Seq[(scala.util.matching.Regex, Set[String], String)] = Seq(
    (
      """Ref(?:\.lens|\.of)?\[\s*(?:cats\.effect\.)?IO\s*,\s*(?:Model\s*,\s*)?AppState\s*\]""".r,
      Set(
        "main/scala/com/serenity/state/manager/ModelCommit.scala",
        "main/scala/com/serenity/state/manager/StateManagerDispatcher.scala"
      ),
      "Ref[IO, AppState] outside the dispatcher/ModelCommit layer -- read an IO[AppState], write through ModelCommit"
    ),
    (
      """Ref(?:\.of)?\[\s*(?:cats\.effect\.)?IO\s*,\s*Model\s*\]""".r,
      Set(
        "main/scala/com/serenity/state/manager/ModelCommit.scala",
        "main/scala/com/serenity/state/manager/StateManagerDispatcher.scala",
        "main/scala/com/serenity/state/manager/StateManagerOperationBoundary.scala",
        "main/scala/com/serenity/state/manager/StateManagerRuntime.scala",
        "main/scala/com/serenity/state/manager/StateManager.scala"
      ),
      "Ref[IO, Model] outside the dispatcher/ModelCommit layer -- take a ModelCommit instead"
    )
  )

  /** #1935: a document's `content` changes only through `Document.withContent` (directly, or through `Buffer`'s
    * edit helpers), which advances `contentVersion` -- the stamp `Buffer.richTextInSync` and the debounced outline
    * re-parse trust instead of re-comparing text. A `document.copy(content = ...)` skips that bump, so it is refused in
    * `src/main` outside the file that defines the sanctioned path. Matched over the whole file rather than per line,
    * since a formatted `copy(` often puts `content =` on a later line.
    */
  val DocumentContentOwner: String = "main/scala/com/serenity/state/models/Buffer.scala"

  private val DocumentCopy = """\bdocument\s*\.\s*copy\s*\(""".r
  private val ContentArgument = """(^|[(,\s])content\s*=(?!=)""".r

  /** The text between the `(` that ends at `start` and its matching `)`. */
  private def balancedArguments(text: String, start: Int): String = {
    var depth = 1
    var i = start
    while (i < text.length && depth > 0) {
      text.charAt(i) match {
        case '(' => depth += 1
        case ')' => depth -= 1
        case _   => ()
      }
      i += 1
    }
    text.substring(start, math.max(start, i - 1))
  }

  private def documentContentViolations(path: String, lines: Vector[String]): Seq[Violation] =
    if (!path.startsWith("main/") || path == DocumentContentOwner) Nil
    else {
      val text = lines.map { line =>
        val trimmed = line.trim
        if (trimmed.startsWith("//") || trimmed.startsWith("*") || trimmed.startsWith("/*")) "" else line
      }.mkString("\n")
      DocumentCopy.findAllMatchIn(text).toSeq.flatMap { found =>
        if (ContentArgument.findFirstIn(balancedArguments(text, found.end)).isEmpty) Nil
        else {
          val line = text.substring(0, found.start).count(_ == '\n') + 1
          Seq(
            Violation(
              path,
              s"document content assigned at line $line: use Document.withContent so contentVersion advances (#1935)",
              1
            )
          )
        }
      }
    }

  /** #1677: no `AtomicReference`, `AtomicInteger`, `synchronized`, or `mutable.` may live inside a top-level
    * `object` declaration, or anywhere under `state/models`, in `src/main`. An `object` compiles to a single
    * JVM-wide instance, so any of these forms held directly in its body -- not inside a nested `class`/`trait`,
    * which owns a separate instance per construction -- is exactly the shared, un-scoped mutable state this issue
    * removed from `RendererFrameState`, `ThemeManager`, `CharacterRenderer`, `MarkdownPreviewCache` and
    * `DictionaryLoader`. The domain model (`state/models`) is held to the same four patterns regardless of
    * object/class, since a case class describing data must never carry a hidden mutable field at all (`AppState`'s
    * old `annotationIndexCache` et al.).
    *
    * Detection is structural, not a flat per-file text search: a running stack of open `object`/`class`/`trait`/
    * `enum` frames (by indentation -- the same technique [[bodyLength]] uses to find where a method ends) tracks
    * what type textually encloses each line, so a legitimately instance-scoped field of a `class` nested inside an
    * `object` is not mistaken for a field of the object itself -- only an unbroken chain of enclosing `object`s all
    * the way up counts as top-level. Block (`/** ... */`) and line (`//`) comments are skipped so a comment that
    * merely explains why a pattern is or isn't used (as this file's own doc comments do) is never itself flagged.
    */
  private val MutabilityTokens: Seq[String] = Seq("AtomicReference", "AtomicInteger", "synchronized", "mutable.")

  private val TypeStart =
    """^(\s*)((?:override|private(?:\[[^\]]+\])?|protected(?:\[[^\]]+\])?|final|sealed|abstract|implicit|case|open)\s+)*(object|class|trait|enum)\b""".r

  final private case class TypeFrame(isObject: Boolean, indent: Int)

  private def mutabilityViolations(path: String, lines: Vector[String]): Seq[Violation] =
    if (!path.startsWith("main/")) Nil
    else {
      val inModels                = path.contains("com/serenity/state/models")
      var stack: List[TypeFrame]  = Nil
      var inBlockComment: Boolean = false

      lines.zipWithIndex.flatMap { case (line, index) =>
        val trimmed = line.trim
        if (inBlockComment) {
          if (trimmed.contains("*/")) inBlockComment = false
          Nil
        } else if (trimmed.isEmpty || trimmed.startsWith("//") || trimmed.startsWith("*")) {
          Nil
        } else if (trimmed.startsWith("/*")) {
          if (!trimmed.contains("*/")) inBlockComment = true
          Nil
        } else {
          val indent = line.indexWhere(!_.isWhitespace)
          stack = stack.dropWhile(_.indent >= indent)

          val violation =
            if (!MutabilityTokens.exists(trimmed.contains)) None
            else if (inModels)
              Some(
                Violation(
                  path,
                  s"forbidden mutable state at line ${index + 1}: state/models must hold no AtomicReference/" +
                    "AtomicInteger/synchronized/mutable. state -- describe data, not hidden mutation (#1677)",
                  1
                )
              )
            else if (stack.nonEmpty && stack.forall(_.isObject))
              Some(
                Violation(
                  path,
                  s"forbidden mutable state at line ${index + 1}: a top-level object may not hold AtomicReference/" +
                    "AtomicInteger/synchronized/mutable. state -- scope it to an owning instance instead (#1677)",
                  1
                )
              )
            else None

          TypeStart.findPrefixMatchOf(line).foreach { m =>
            stack = TypeFrame(isObject = m.group(3) == "object", indent) :: stack
          }

          violation.toSeq
        }
      }
    }

  final case class Violation(path: String, detail: String, measured: Int) {
    def key: String = s"$path\t$detail"
    def render: String = s"$key\t$measured"
  }

  private val MethodStart =
    """^(\s*)((?:override|private|protected|final|inline|transparent|implicit)\s+|\[[^\]]*\]\s+)*def\s+(\w+)""".r

  private def readLines(file: File): Vector[String] = {
    val source = Source.fromFile(file, "UTF-8")
    try source.getLines().toVector
    finally source.close()
  }

  private def scalaFiles(base: File): Seq[File] =
    (base ** "*.scala").get().sortBy(_.getPath)

  private def relativise(base: File, file: File): String =
    base.toPath.relativize(file.toPath).toString.replace('\\', '/')

  /** Lines from `start` until indentation returns to `indent` or shallower, ignoring blanks and comments. */
  private def bodyLength(lines: Vector[String], start: Int, indent: Int): Int = {
    var end = lines.length
    var i = start + 1
    var found = false
    while (i < lines.length && !found) {
      val line = lines(i)
      val trimmed = line.trim
      if (trimmed.nonEmpty && !trimmed.startsWith("//")) {
        val current = line.indexWhere(!_.isWhitespace)
        if (current <= indent) { end = i; found = true }
      }
      i += 1
    }
    end - start
  }

  private def methodViolations(path: String, lines: Vector[String]): Seq[Violation] =
    lines.zipWithIndex.collect {
      case (line, index) if MethodStart.findPrefixMatchOf(line).isDefined =>
        val indent = line.indexWhere(!_.isWhitespace)
        val name = MethodStart.findPrefixMatchOf(line).map(_.group(3)).getOrElse("?")
        (index, indent, name)
    }.flatMap { case (index, indent, name) =>
      val length = bodyLength(lines, index, indent)
      if (length > MaxMethodLines) Some(Violation(path, s"method $name", length)) else None
    }

  /** Matches a forbidden package/class anywhere in a non-comment line, not only on an `import` line -- a
    * fully-qualified reference (`com.serenity.state.manager.EditorGeometryProducer.forPane(...)`) reaches the
    * same forbidden code an `import` would, and evades a check that only looks at `import` lines (#1676). Doc
    * comments that merely mention a forbidden package by name (as this file's own header does) are excluded the
    * same way [[callViolations]] already excludes them.
    */
  private def importViolations(path: String, lines: Vector[String]): Seq[Violation] =
    ForbiddenImports.flatMap { rule =>
      if (!path.contains(rule.pkg) || (rule.mainOnly && !path.startsWith("main/"))) Nil
      else
        lines.zipWithIndex.collect {
          case (line, index)
              if !line.trim.startsWith("//") && !line.trim.startsWith("*") &&
                rule.forbidden.exists(f => line.contains(f)) =>
            Violation(path, s"forbidden reference at line ${index + 1}: ${rule.reason}", 1)
        }
    }

  private def callViolations(path: String, lines: Vector[String]): Seq[Violation] =
    ForbiddenCalls.flatMap { case (scope, forbidden, reason) =>
      if (!path.contains(scope)) Nil
      else
        lines.zipWithIndex.collect {
          case (line, index)
              if !line.trim.startsWith("*") && !line.trim.startsWith("//") &&
                forbidden.exists(f => line.contains(f)) =>
            Violation(path, s"forbidden call at line ${index + 1}: $reason", 1)
        }
    }

  private def stateOwnershipViolations(path: String, lines: Vector[String]): Seq[Violation] =
    if (!path.startsWith("main/")) Nil
    else
      StateOwnership.flatMap { case (pattern, allowed, reason) =>
        if (allowed.contains(path)) Nil
        else
          lines.zipWithIndex.collect {
            case (line, index)
                if !line.trim.startsWith("*") && !line.trim.startsWith("//") &&
                  pattern.findFirstIn(line).isDefined =>
              Violation(path, s"state ownership at line ${index + 1}: $reason", 1)
          }
      }

  def collect(base: File): Seq[Violation] =
    scalaFiles(base).flatMap { file =>
      val path = relativise(base, file)
      val lines = readLines(file)
      val fileViolation =
        if (lines.length > MaxFileLines) Seq(Violation(path, "file length", lines.length)) else Nil
      fileViolation ++ methodViolations(path, lines) ++ importViolations(path, lines) ++ callViolations(path, lines) ++
        stateOwnershipViolations(path, lines) ++ mutabilityViolations(path, lines) ++
        documentContentViolations(path, lines)
    }

  def readBaseline(file: File): Map[String, Int] =
    if (!file.exists()) Map.empty
    else
      readLines(file)
        .filter(line => line.trim.nonEmpty && !line.trim.startsWith("#"))
        .flatMap { line =>
          line.split('\t') match {
            case Array(path, detail, measured) => Some(s"$path\t$detail" -> measured.trim.toInt)
            case _                             => None
          }
        }
        .toMap

  def writeBaseline(file: File, violations: Seq[Violation]): Unit = {
    val header =
      Seq(
        "# Architecture-check baseline. Generated by `sbt writeArchitectureBaseline`.",
        s"# Targets: method <= $MaxMethodLines lines, file <= $MaxFileLines lines, no forbidden imports or calls.",
        "# This list may shrink, never grow. Removing an entry is the point.",
        "# Growth must be justified: CI fails a PR that grows this file's entry count or total measured",
        "# lines/hits unless the same diff adds a '# paydown: <why, issue link>' comment line (see #1413).",
        "# Columns: path <TAB> detail <TAB> measured"
      )
    // The header above documents the paydown-comment rule, but the check itself isn't done here: the
    // requirement that baseline growth carry a "# paydown: <why, issue link>" line is enforced by the
    // "Require justification for baseline growth" step in .github/workflows/ci.yml.
    IO.write(file, (header ++ violations.map(_.render)).mkString("", "\n", "\n"))
  }

  def check(base: File, baselineFile: File): Option[String] = {
    val baseline = readBaseline(baselineFile)
    val current = collect(base)
    val currentByKey = current.map(v => v.key -> v.measured).toMap

    val added = current.filterNot(v => baseline.contains(v.key))
    val worsened = current.filter(v => baseline.get(v.key).exists(_ < v.measured))
    val fixed = baseline.keySet.diff(currentByKey.keySet)

    val problems =
      (if (added.isEmpty) Nil
       else
         Seq(
           s"${added.size} new violation(s) -- keep new code under target:",
           added.map(v => s"  ${v.path}  ${v.detail}  ${v.measured}").mkString("\n")
         )) ++
        (if (worsened.isEmpty) Nil
         else
           Seq(
             s"${worsened.size} existing violation(s) got worse:",
             worsened
               .map(v => s"  ${v.path}  ${v.detail}  ${baseline(v.key)} -> ${v.measured}")
               .mkString("\n")
           )) ++
        (if (fixed.isEmpty) Nil
         else
           Seq(
             s"${fixed.size} baseline entr(y/ies) no longer apply -- run `sbt writeArchitectureBaseline` to bank the win:",
             fixed.toSeq.sorted.map(key => s"  ${key.replace('\t', ' ')}").mkString("\n")
           ))

    if (problems.isEmpty) None else Some(problems.mkString("\n"))
  }

  /** Regression guard for the checks themselves (#1676): a reducer that reaches a forbidden package through a
    * fully-qualified reference rather than an `import` line must still be caught, a plain `import` violation must
    * still be caught (widening the match can't be allowed to narrow it), and a comment that merely mentions a
    * forbidden package must not be. Plain `require` assertions rather than a test framework -- `project/` sources
    * have no test dependency of their own -- run from the `architectureChecksSelfTest` sbt task, which
    * `architectureCheck` depends on, so CI fails immediately if `importViolations`'s matching is ever narrowed back.
    */
  def selfTest(): Unit = {
    def check(
        description: String,
        lines: Vector[String],
        expectCaught: Boolean,
        path: String = "main/scala/com/serenity/state/reducers/Sample.scala"
    ): Unit = {
      val violations = importViolations(path, lines)
      require(
        violations.nonEmpty == expectCaught,
        s"ArchitectureChecks self-test failed ($description): expected a forbidden reference to be " +
          s"${if (expectCaught) "caught" else "ignored"}, got ${violations.size} violation(s)"
      )
    }

    check(
      "fully-qualified state.manager call outside an import line",
      Vector(
        "package com.serenity.state.reducers",
        "object Sample:",
        "  def x(state: Int) = com.serenity.state.manager.EditorGeometryProducer.forPane(state, 0)"
      ),
      expectCaught = true
    )

    check(
      "fully-qualified java.awt reference outside an import line",
      Vector(
        "package com.serenity.state.reducers",
        "object Sample:",
        "  def color: java.awt.Color = java.awt.Color.RED"
      ),
      expectCaught = true
    )

    check(
      "an ordinary forbidden import line",
      Vector(
        "package com.serenity.state.reducers",
        "import com.serenity.ui.fonts.FontLoader"
      ),
      expectCaught = true
    )

    check(
      "a comment that only mentions a forbidden package by name",
      Vector(
        "package com.serenity.state.reducers",
        "// see com.serenity.state.manager.EditorGeometryProducer for why this stays at the effect boundary"
      ),
      expectCaught = false
    )

    check(
      "a test spec driving the effect-boundary manager to simulate the full post-reduce pipeline",
      Vector(
        "package com.serenity.state.reducers",
        "import com.serenity.state.manager.CursorViewport"
      ),
      expectCaught = false,
      path = "test/scala/com/serenity/state/reducers/SampleSpec.scala"
    )

    check(
      "a test spec driving another effect-boundary helper (ui.fonts) from the same main-only rule",
      Vector(
        "package com.serenity.state.reducers",
        "import com.serenity.ui.fonts.FontLoader"
      ),
      expectCaught = false,
      path = "test/scala/com/serenity/state/reducers/SampleSpec.scala"
    )

    check(
      "production code still catches an ordinary forbidden import (java.awt) under the main-only scoping",
      Vector(
        "package com.serenity.state.reducers",
        "import com.serenity.ui.fonts.FontLoader"
      ),
      expectCaught = true
    )

    def checkDispatcherPath(description: String, line: String, path: String, expectCaught: Boolean): Unit = {
      val violations = callViolations(path, Vector(line))
      require(
        violations.nonEmpty == expectCaught,
        s"ArchitectureChecks self-test failed ($description): expected a blocking call to be " +
          s"${if (expectCaught) "caught" else "ignored"}, got ${violations.size} violation(s)"
      )
    }

    checkDispatcherPath(
      "a blocking effect in the event pipeline",
      "    cats.effect.IO.blocking(readIndex())",
      DispatcherPathFiles.head,
      expectCaught = true
    )

    checkDispatcherPath(
      "a java.nio Files call in the event pipeline",
      "    val present = Files.exists(path)",
      DispatcherPathFiles.head,
      expectCaught = true
    )

    checkDispatcherPath(
      "a blocking effect in a lane job's own file",
      "    IO.blocking(readIndex())",
      "main/scala/com/serenity/state/manager/CommandRunnerOpening.scala",
      expectCaught = false
    )

    def checkMutability(
        description: String,
        lines: Vector[String],
        expectCaught: Boolean,
        path: String = "main/scala/com/serenity/ui/renderer/Sample.scala"
    ): Unit = {
      val violations = mutabilityViolations(path, lines)
      require(
        violations.nonEmpty == expectCaught,
        s"ArchitectureChecks self-test failed ($description): expected forbidden mutable state to be " +
          s"${if (expectCaught) "caught" else "ignored"}, got ${violations.size} violation(s)"
      )
    }

    checkMutability(
      "an AtomicReference field held directly by a top-level object",
      Vector(
        "package com.serenity.ui.renderer",
        "object Sample:",
        "  private val cache = new java.util.concurrent.atomic.AtomicReference[Int](0)"
      ),
      expectCaught = true
    )

    checkMutability(
      "a synchronized call against a field held directly by a top-level object",
      Vector(
        "package com.serenity.ui.renderer",
        "object Sample:",
        "  private val lock = new Object",
        "  def touch(): Unit = lock.synchronized { () }"
      ),
      expectCaught = true
    )

    checkMutability(
      "a legitimate instance-owned mutable field inside a class nested in an object",
      Vector(
        "package com.serenity.ui.renderer",
        "object Sample:",
        "  final private class Cache:",
        "    private val ref = new java.util.concurrent.atomic.AtomicReference[Int](0)"
      ),
      expectCaught = false
    )

    checkMutability(
      "a line comment that only mentions AtomicReference by name",
      Vector(
        "package com.serenity.ui.renderer",
        "object Sample:",
        "  // conceptually similar to an AtomicReference-based CAS loop, but isn't one",
        "  private val x = 1"
      ),
      expectCaught = false
    )

    checkMutability(
      "a doc-comment opening line mentioning AtomicReference by name",
      Vector(
        "package com.serenity.ui.renderer",
        "/** Bounded, `AtomicReference`-backed cache (kept only in this comment). */",
        "object Sample:",
        "  private val x = 1"
      ),
      expectCaught = false
    )

    checkMutability(
      "state/models forbids the four patterns regardless of object/class",
      Vector(
        "package com.serenity.state.models",
        "final case class Sample(",
        "  private val cache: java.util.concurrent.atomic.AtomicReference[Int]",
        ")"
      ),
      expectCaught = true,
      path = "main/scala/com/serenity/state/models/Sample.scala"
    )

    checkMutability(
      "the top-level-object rule is scoped to src/main, not test sources",
      Vector(
        "package com.serenity.ui.renderer",
        "object Sample:",
        "  private val cache = new java.util.concurrent.atomic.AtomicReference[Int](0)"
      ),
      expectCaught = false,
      path = "test/scala/com/serenity/ui/renderer/SampleSpec.scala"
    )

    def checkDocumentContent(
        description: String,
        lines: Vector[String],
        expectCaught: Boolean,
        path: String = "main/scala/com/serenity/state/reducers/Sample.scala"
    ): Unit = {
      val violations = documentContentViolations(path, lines)
      require(
        violations.nonEmpty == expectCaught,
        s"ArchitectureChecks self-test failed ($description): expected a document content assignment to be " +
          s"${if (expectCaught) "caught" else "ignored"}, got ${violations.size} violation(s)"
      )
    }

    checkDocumentContent(
      "a document copy that sets content on one line",
      Vector("  val next = buffer.document.copy(content = edited, isDirty = true)"),
      expectCaught = true
    )

    checkDocumentContent(
      "a document copy that sets content on a later line",
      Vector("  val next = buffer.document.copy(", "    isDirty = true,", "    content = edited", "  )"),
      expectCaught = true
    )

    checkDocumentContent(
      "a document copy that leaves content alone",
      Vector("  val next = buffer.document.copy(language = language(content == other))"),
      expectCaught = false
    )

    checkDocumentContent(
      "a surface copy whose own field is called content",
      Vector("  val next = surface.copy(content = kept)"),
      expectCaught = false
    )

    checkDocumentContent(
      "the file that defines the sanctioned path",
      Vector("  def withContent(newContent: Rope): Document = document.copy(content = newContent)"),
      expectCaught = false,
      path = DocumentContentOwner
    )

    checkDocumentContent(
      "a test fixture building a buffer with some text",
      Vector("  val fixture = buffer.document.copy(content = Rope(text))"),
      expectCaught = false,
      path = "test/scala/com/serenity/state/reducers/SampleSpec.scala"
    )
  }
}
