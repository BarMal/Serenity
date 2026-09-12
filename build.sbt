import sbtassembly.AssemblyPlugin.autoImport.*
import sbtassembly.MergeStrategy

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.9.0"

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

Compile / scalacOptions ++= Seq(
  "-Wunused:all",
  "-Wvalue-discard",
  "-Wnonunit-statement",
  // Exhaustivity on a sealed hierarchy is only a warning, so without -Werror, sealing an ADT still lets a missing
  // case compile. Turning it on is the natural completion of the sealing work. (-Xfatal-warnings is the historical
  // alias and is deprecated as of Scala 3.8; -Werror is the spelling to use.)
  //
  // This used to be deferred behind a backlog of 150 warnings -- 105 staged WartRemover findings, 36 unused symbols,
  // 4 non-exhaustive matches, 2 potential-issue, 1 deprecation. That backlog is gone: a clean compile now reports
  // zero warnings, so -Werror holds a line already reached rather than demanding a migration first.
  //
  // One consequence to know: under -Werror, whether a wart is registered as an error or a warning only changes the
  // message, not the outcome -- both fail the build. The four warts that used to be staged as warnings (#1449:
  // Wart.Null, Wart.Throw, Wart.OptionPartial, Wart.IterableOps) have since been promoted to wartremoverErrors
  // outright, once an audit confirmed their only usage sits inside the packages wartremoverExcluded already drops
  // from wart checking entirely.
  "-Werror"
)

Test / scalacOptions --= Seq(
  "-Wunused:all",
  "-Wvalue-discard",
  "-Wnonunit-statement"
)

Test / scalacOptions ++= Seq(
  "-Wunused:imports"
)

Compile / run / fork := true

// Structural checks the compiler and WartRemover cannot express: method length, file length, and the
// layering rules the package structure implies. Both size checks are ratchets against a checked-in
// baseline -- see project/ArchitectureChecks.scala.
lazy val architectureBaselineFile = settingKey[File]("Baseline of known architecture-check violations")
lazy val architectureCheck        = taskKey[Unit]("Fail if any file or method grew past target, or a layer was crossed")
lazy val writeArchitectureBaseline = taskKey[Unit]("Regenerate the architecture-check baseline from the current tree")

lazy val root = (project in file("."))
  .settings(
    name := "Serenity",
    // WartRemover encodes rules docs/coding-standards.md and CLAUDE.md already state in prose.
    // Main sources only: tests legitimately use throw/null/partial access to build failure fixtures,
    // mirroring how Test / scalacOptions already relaxes the -W flags above.
    //
    // Errors are the enforced set. Anything needing a migration first would stay a warning until the
    // violations are cleared, then move up -- but there is currently nothing in wartremoverWarnings, since
    // the last staged batch (#1449) has been promoted below. The plugin is CrossVersion.full, so a Scala
    // upgrade needs a matching WartRemover release -- 3.6.1 publishes for 3.8.4.
    Compile / wartremoverErrors ++= Seq(
      // Type-level closure: case classes are final, and nobody re-opens a sealed hierarchy by
      // extending one of its cases with a non-final class.
      Wart.FinalCaseClass,
      Wart.LeakingSealed,
      // Already at zero violations; enabled so they cannot regress.
      Wart.TripleQuestionMark, // CLAUDE.md: "No stub implementations -- no ???"
      Wart.ThreadSleep,        // CLAUDE.md: "No Thread.sleep inside IO -- use IO.sleep"
      // Cleared during this change: the remaining casts were replaced by type ascription, and the
      // isInstanceOf checks by named pattern-matching predicates.
      Wart.AsInstanceOf,
      Wart.IsInstanceOf,
      // Promoted from wartremoverWarnings (#1449): an audit confirmed all current usage of these four
      // patterns is concentrated in the four packages listed in wartremoverExcluded below. That exclusion
      // list is a file-level filter applied ahead of severity -- it drops those sources from wart checking
      // entirely, for both wartremoverErrors and wartremoverWarnings alike -- so promoting these warts to
      // Errors does not touch the legitimate interop code in those packages; it only turns on enforcement
      // everywhere else, where the audit found zero violations.
      Wart.Null,
      Wart.Throw,
      Wart.OptionPartial,
      Wart.IterableOps
    ),
    // Deliberately absent from both wartremoverErrors and wartremoverWarnings: Wart.MutableDataStructures. It flags
    // StringBuilder, which docs/coding-standards.md explicitly permits ("Private local mutation is acceptable when
    // it is contained and earns its place, for example StringBuilder during rendering or rope traversal"). The large
    // majority of hits here are exactly that sanctioned use, so the wart would fight the standard rather than
    // enforce it. Contained mutation is already governed by DisableSyntax.noVars.
    // Tests are exempt, the same way Test / scalacOptions already drops the -W flags above. Test code
    // legitimately uses casts, throws and partial access to build failure fixtures and to assert on
    // representation invariants -- RopeMetadataAndTraversalSpec, for instance, subclasses Leaf to prove that search never
    // materialises the whole rope.
    //
    // Scoping wartremoverErrors to Compile is not enough: the plugin contributes its traversers at a
    // scope Test delegates from, so they reappear in Test / scalacOptions. Strip them there directly,
    // mirroring how this build already removes the -W flags from Test. -Xplugin stays and is inert
    // once no traverser is enabled.
    Test / scalacOptions ~= (_.filterNot(_.startsWith("-P:wartremover:"))),
    // Java/AWT interop and byte-level protocol framing genuinely need the escape hatches above.
    // Set unscoped: the plugin reads this outside the Compile scope.
    wartremoverExcluded ++= Seq(
      baseDirectory.value / "src" / "main" / "scala" / "com" / "serenity" / "ui" / "terminal",
      baseDirectory.value / "src" / "main" / "scala" / "com" / "serenity" / "ui" / "accessibility",
      baseDirectory.value / "src" / "main" / "scala" / "com" / "serenity" / "lsp" / "client",
      baseDirectory.value / "src" / "main" / "scala" / "com" / "serenity" / "richtext"
    ),
    architectureBaselineFile := baseDirectory.value / "project" / "architecture-baseline.tsv",
    architectureCheck := {
      val log = streams.value.log
      ArchitectureChecks.check(baseDirectory.value / "src", architectureBaselineFile.value) match {
        case None =>
          log.info("architectureCheck: ratchet holds")
        case Some(report) =>
          sys.error(
            s"""architectureCheck failed.
               |
               |$report
               |
               |Targets: method <= ${ArchitectureChecks.MaxMethodLines} lines, file <= ${ArchitectureChecks.MaxFileLines} lines.
               |Split the offending code, or if you deliberately accept it, run `sbt writeArchitectureBaseline`
               |and explain the new entry in review.""".stripMargin
          )
      }
    },
    writeArchitectureBaseline := {
      val violations = ArchitectureChecks.collect(baseDirectory.value / "src")
      ArchitectureChecks.writeBaseline(architectureBaselineFile.value, violations)
      streams.value.log.info(
        s"architecture baseline written: ${violations.size} entries at ${architectureBaselineFile.value}"
      )
    },
    // The commit this build was made from, generated into a source file so anything that reports a
    // build identity -- `--version`, an about surface -- reads one value rather than inventing its own.
    // Falls back to "unknown" outside a git checkout (a source tarball, say) rather than failing the build.
    Compile / sourceGenerators += Def.task {
      val generated = (Compile / sourceManaged).value / "com" / "serenity" / "BuildInfo.scala"
      val commit =
        scala.util.Try(scala.sys.process.Process("git rev-parse HEAD").!!.trim).filter(_.nonEmpty).getOrElse("unknown")
      IO.write(
        generated,
        s"""package com.serenity
           |
           |/** Generated by build.sbt. Do not edit. */
           |object BuildInfo:
           |  val commit: String  = "$commit"
           |  val version: String = "${version.value}"
           |""".stripMargin
      )
      Seq(generated)
    }.taskValue,
    Compile / mainClass := Some("Main"),
    assembly / mainClass := Some("Main"),
    assembly / assemblyJarName := "Serenity.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "services", _*) => MergeStrategy.concat
      case x @ PathList("META-INF", xs @ _*) =>
        xs.map(_.toLowerCase) match {
          case "manifest.mf" :: Nil   => MergeStrategy.discard
          case "index.list" :: Nil    => MergeStrategy.discard
          case "dependencies" :: Nil  => MergeStrategy.discard
          case name :: Nil
              if name.endsWith(".sf") || name.endsWith(".rsa") || name.endsWith(".dsa") || name.endsWith(".ec") =>
            MergeStrategy.discard
          case _ => (assembly / assemblyMergeStrategy).value(x)
        }
      case PathList("module-info.class") => MergeStrategy.discard
      case x => (assembly / assemblyMergeStrategy).value(x)
    },
    Test / testOptions ++= Seq(
      Tests.Setup(() => System.setProperty("serenity.test.ephemeralSessions", "true")),
      Tests.Cleanup(() => System.clearProperty("serenity.test.ephemeralSessions"))
    ),
    // Real-OS-boundary specs (a genuine loopback socket, a genuine sun.misc.Signal.raise -- see
    // com.serenity.testkit.RealBoundaryTest's doc comment) are excluded from `sbt test`'s discovery of the whole
    // suite, but not from `testOnly`: this is scoped to the `test` task specifically (`Test / test / testOptions`),
    // one level more specific than the plain `Test / testOptions` above that `testOnly` still falls back to
    // unfiltered, so `realBoundaryTest` below can name these two specs directly and still run them.
    Test / test / testOptions := (Test / testOptions).value :+
      Tests.Argument(TestFrameworks.ScalaTest, "-l", "com.serenity.testkit.RealBoundaryTest")
  )

// Runs only the two real-OS-boundary specs (a real loopback socket, a real OS signal delivered via
// sun.misc.Signal.raise) that `sbt test` excludes -- see RealBoundaryTest's doc comment for why they must stay out
// of the fast parallel suite. Named directly rather than by tag, since `Test / test`'s "-l" exclusion above does not
// apply to `testOnly`.
lazy val realBoundaryTest = taskKey[Unit]("Run the real-OS-boundary integration specs excluded from `sbt test`")

realBoundaryTest := (Test / testOnly)
  .toTask(
    " com.serenity.lsp.LspConnectionRealSocketIntegrationSpec com.serenity.ui.tui.TerminalShellRealSignalIntegrationSpec"
  )
  .value

libraryDependencies ++= Seq(
  "org.typelevel"         %% "cats-effect"     % "3.7.1",
  "co.fs2"                %% "fs2-core"        % "3.13.0",
  "co.fs2"                %% "fs2-io"          % "3.13.0",
  "org.scalatest"         %% "scalatest"       % "3.2.19" % "test",
  // Drives IO programs on a mocked scheduler/clock for deterministic tests of timing-dependent code (timeouts,
  // intervals, idle cadence) -- see com.serenity.testkit.VirtualTime.
  "org.typelevel"         %% "cats-effect-testkit" % "3.7.1" % "test",
  // Property and law testing, test scope only -- the assembled JAR is unchanged.
  // scalatestplus is pinned to the release matching ScalaTest 3.2.19 and brings ScalaCheck with it.
  // cats-laws tracks the cats-core 2.13.0 that cats-effect 3.7.1 already resolves, so no eviction.
  "org.scalatestplus"     %% "scalacheck-1-18" % "3.2.19.0" % "test",
  "org.typelevel"         %% "cats-laws"       % "2.13.0"   % "test",
  "org.typelevel"         %% "discipline-scalatest" % "2.3.0" % "test",
  "com.github.pureconfig" %% "pureconfig-core" % "0.17.10",
  // Launch-argument parsing. decline-effect is only for its cats-effect glue; the command itself is plain decline,
  // so `LaunchOptions.parse` stays a pure function over args that specs can call directly.
  "com.monovore"          %% "decline"         % "2.6.2",
  "com.monovore"          %% "decline-effect"  % "2.6.2"
)

val circeVersion = "0.14.16"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core"    % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser"  % circeVersion
)

val log4CatsVersion = "2.8.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "log4cats-core"   % log4CatsVersion,
  "org.typelevel" %% "log4cats-slf4j"  % log4CatsVersion,
  "ch.qos.logback" % "logback-classic" % "1.5.38",
  "net.java.dev.jna" % "jna-platform"  % "5.19.1"
)

val commonMarkVersion = "0.30.0"

libraryDependencies ++= Seq(
  "org.commonmark"   % "commonmark"                   % commonMarkVersion,
  "org.commonmark"   % "commonmark-ext-gfm-tables"    % commonMarkVersion,
  "org.commonmark"   % "commonmark-ext-task-list-items" % commonMarkVersion,
  "org.xhtmlrenderer" % "flying-saucer-core"          % "10.2.2"
)

val jlineVersion = "3.30.16"

// TUI shell (#1107): raw mode, alternate screen, resize signals, terminal size. jline-terminal-jni is the modern
// pure-JNI native provider (no external jna/jansi native libs to manage) -- jline-reader is deliberately not a
// dependency, since this codebase does its own raw terminal I/O rather than JLine's line-editing/completion stack.
libraryDependencies ++= Seq(
  "org.jline" % "jline-terminal"     % jlineVersion,
  "org.jline" % "jline-terminal-jni" % jlineVersion
)

// Unifies grapheme-cluster, line-break and East-Asian-Width segmentation on one versioned Unicode table (#1277),
// replacing three independent hand-rolled approximations that could (and did, see #1271) disagree with each other.
// Dependency only in this step -- see #1277 step 1 for the assembled-JAR size measurement that gated this addition.
libraryDependencies += "com.ibm.icu" % "icu4j" % "78.3"
