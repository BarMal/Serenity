import sbtassembly.AssemblyPlugin.autoImport.*
import sbtassembly.MergeStrategy
import com.serenity.release.PackageVersion

ThisBuild / scalaVersion := "3.9.0"

ThisBuild / licenses := Seq("GPL-3.0-or-later" -> url("https://www.gnu.org/licenses/gpl-3.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/BarMal/Serenity"))

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

lazy val architectureChecksSelfTest = taskKey[Unit](
  "Regression guard for ArchitectureChecks itself: confirms forbidden fully-qualified references are still caught (#1676)"
)

// THIRD-PARTY-NOTICES.md is rendered from the resolved runtime classpath plus the registry in third-party/, so a
// dependency with no licence entry fails the build rather than shipping unattributed (#2019).
lazy val thirdPartyNotices =
  taskKey[String]("Render THIRD-PARTY-NOTICES.md from the runtime classpath and third-party/")
lazy val generateThirdPartyNotices = taskKey[Unit]("Rewrite THIRD-PARTY-NOTICES.md at the repo root")
lazy val checkThirdPartyNotices =
  taskKey[Unit]("Fail if THIRD-PARTY-NOTICES.md is stale or a runtime module has no licence entry")

// The version comes from the annotated vX.Y.Z tag via sbt-dynver, and the numeric form a packager needs from
// PackageVersion (project/PackageVersion.scala). Both read git, so a source tarball or a shallow clone with no tag
// degrades to dynver's untagged version and to 1.0.0/dev rather than failing the build.
lazy val gitDescribe = taskKey[String]("git describe over vX.Y.Z tags; the bare sha, or empty, when none is reachable")
lazy val packageVersion =
  taskKey[Either[String, PackageVersion.Result]]("Numeric package version and channel for this checkout")
lazy val writePackageVersion =
  taskKey[File]("Write target/package/app-version.txt, the value jpackage --app-version must receive")

def gitOutput(root: File, args: String*): Option[String] =
  scala.util
    .Try(scala.sys.process.Process("git" +: args, root).!!(scala.sys.process.ProcessLogger(_ => ())).trim)
    .toOption
    .filter(_.nonEmpty)

lazy val root = (project in file("."))
  .settings(
    // The package version module is build code under project/; compile it into the tests too so a spec can reach it.
    Test / unmanagedSources += baseDirectory.value / "project" / "PackageVersion.scala",
    gitDescribe := gitOutput(
      baseDirectory.value,
      "describe",
      "--tags",
      "--long",
      "--abbrev=8",
      "--match",
      "v[0-9]*",
      "--always",
      "--dirty"
    ).getOrElse(""),
    packageVersion := PackageVersion.fromDescribe(gitDescribe.value, sys.env.get("SERENITY_NIGHTLY").contains("true")),
    writePackageVersion := {
      val result = packageVersion.value.fold(sys.error(_), identity)
      val out    = target.value / "package" / "app-version.txt"
      IO.write(out, result.numeric)
      streams.value.log.info(s"package version ${result.numeric} (${result.channel.label}) written to $out")
      out
    },
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
    architectureChecksSelfTest := {
      ArchitectureChecks.selfTest()
      streams.value.log.info("architectureChecksSelfTest: ratchet's own matching still catches FQN evasion")
    },
    architectureCheck := {
      architectureChecksSelfTest.value
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
    // Build identity, generated into a source file so anything that reports it -- `--version`, an about surface --
    // reads one value rather than inventing its own. commitTime is the commit's date, not the wall clock, so the
    // same commit always generates the same file. Outside a git checkout every git-derived field falls back
    // ("unknown", 1.0.0/dev) rather than failing the build; only writePackageVersion is strict.
    Compile / sourceGenerators += Def.task {
      val generated  = (Compile / sourceManaged).value / "com" / "serenity" / "BuildInfo.scala"
      val root       = baseDirectory.value
      val log        = streams.value.log
      val commit     = gitOutput(root, "rev-parse", "HEAD").getOrElse("unknown")
      val commitTime = gitOutput(root, "log", "-1", "--format=%cI").getOrElse("unknown")
      val resolved = packageVersion.value.fold(
        problem => {
          log.warn(s"BuildInfo falls back to ${PackageVersion.FirstVersion}/dev: $problem")
          PackageVersion.Result(PackageVersion.FirstVersion, PackageVersion.Channel.Dev)
        },
        identity
      )
      IO.write(
        generated,
        s"""package com.serenity
           |
           |/** Generated by build.sbt. Do not edit. */
           |object BuildInfo:
           |  val commit: String         = "$commit"
           |  val commitTime: String     = "$commitTime"
           |  val version: String        = "${version.value}"
           |  val packageVersion: String = "${resolved.numeric}"
           |  val channel: String        = "${resolved.channel.label}"
           |""".stripMargin
      )
      Seq(generated)
    }.taskValue,
    thirdPartyNotices := ThirdPartyNotices
      .render(baseDirectory.value.toPath, ThirdPartyNotices.runtimeModulesOf((Runtime / managedClasspath).value))
      .fold(sys.error(_), identity),
    generateThirdPartyNotices := {
      val target = baseDirectory.value / "THIRD-PARTY-NOTICES.md"
      IO.write(target, thirdPartyNotices.value)
      streams.value.log.info(s"wrote $target")
    },
    checkThirdPartyNotices := {
      val committed = IO.read(baseDirectory.value / "THIRD-PARTY-NOTICES.md")
      if (committed != thirdPartyNotices.value)
        sys.error("THIRD-PARTY-NOTICES.md is out of date. Run `sbt generateThirdPartyNotices` and commit the result.")
      streams.value.log.info("checkThirdPartyNotices: notices cover every runtime module")
    },
    // Packaged under META-INF/serenity so the licence reaches the classpath, the assembled JAR and the About
    // command from one copy; the root files stay the single source.
    Compile / resourceGenerators += Def.task {
      val dir = (Compile / resourceManaged).value / "META-INF" / "serenity"
      IO.copyFile(baseDirectory.value / "LICENSE", dir / "LICENSE")
      IO.write(dir / "THIRD-PARTY-NOTICES.md", thirdPartyNotices.value)
      IO.copyFile(baseDirectory.value / "docs" / "PRIVACY.md", dir / "PRIVACY.md")
      Seq(dir / "LICENSE", dir / "THIRD-PARTY-NOTICES.md", dir / "PRIVACY.md")
    }.taskValue,
    // The runtime module list the notices spec checks the shipped notices against.
    Test / resourceGenerators += Def.task {
      val file = (Test / resourceManaged).value / "licences" / "runtime-modules.txt"
      IO.write(
        file,
        ThirdPartyNotices.runtimeModulesOf((Runtime / managedClasspath).value).map(_.key).distinct.sorted.mkString("\n")
      )
      Seq(file)
    }.taskValue,
    // The classpath the kill-recovery spec launches its child JVM with: sbt runs specs in its own JVM, whose
    // `java.class.path` is not the project's.
    Test / resourceGenerators += Def.task {
      val file      = (Test / resourceManaged).value / "crash" / "test-classpath.txt"
      val classpath = (Test / classDirectory).value +: (Test / dependencyClasspath).value.files
      IO.write(file, classpath.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator))
      Seq(file)
    }.taskValue,
    Compile / mainClass        := Some("Main"),
    assembly / mainClass       := Some("Main"),
    assembly / assemblyJarName := "Serenity.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "serenity", _*) => MergeStrategy.first
      case PathList("META-INF", "services", _*) => MergeStrategy.concat
      // Multi-release module descriptors (slf4j-api, commons-logging via FontBox): meaningless in a fat jar.
      case PathList("META-INF", "versions", _, "module-info.class") => MergeStrategy.discard
      case x @ PathList("META-INF", xs @ _*) =>
        xs.map(_.toLowerCase) match {
          case "manifest.mf" :: Nil  => MergeStrategy.discard
          case "index.list" :: Nil   => MergeStrategy.discard
          case "dependencies" :: Nil => MergeStrategy.discard
          case name :: Nil
              if name.endsWith(".sf") || name.endsWith(".rsa") || name.endsWith(".dsa") || name.endsWith(".ec") =>
            MergeStrategy.discard
          case _ => (assembly / assemblyMergeStrategy).value(x)
        }
      case PathList("module-info.class") => MergeStrategy.discard
      case x                             => (assembly / assemblyMergeStrategy).value(x)
    },
    Test / testOptions ++= Seq(
      // One folder per test run holds everything the suites create (`TestTemp`, and the session folder of every
      // `StateManager` built without an explicit one). Removing it here is what stops a run leaving thousands of
      // entries in the system temp directory, whether the suites passed, failed or were cancelled.
      Tests.Setup(() => System.setProperty("serenity.test.tempRoot", IO.createTemporaryDirectory.getAbsolutePath)),
      Tests.Cleanup { () =>
        Option(System.clearProperty("serenity.test.tempRoot")).foreach(root => IO.delete(file(root)))
      },
      // Full stack traces on every failure (ScalaTest's default reporter otherwise truncates them). #1213's
      // macOS-arm64/windows-x64 CI failures show only a bare "Failed tests:" summary line -- no assertion message, no
      // stack trace, for either the failed or the canceled test alongside it -- which makes them undiagnosable from
      // the log as things stand. This does not itself explain that failure; it exists so the next occurrence prints
      // one.
      Tests.Argument(TestFrameworks.ScalaTest, "-oF"),
      // Investigating the #1213-class failures directly against `desktop-publish`'s CI job logs (run 35203534197)
      // found that even with -oF, the GitHub Actions console log for a Windows/macOS `sbt test` run does not carry
      // the per-test assertion/stack-trace detail through to what the API serves back -- only suite names and the
      // aggregate pass/fail/canceled counts survive, for reasons that appear to sit in how that console output is
      // captured/relayed rather than in ScalaTest's own reporting. A JUnit XML report is a second, independent sink
      // for the same failure detail that does not depend on the console log at all -- see the corresponding
      // "Upload test reports" step in .github/workflows/desktop-publish.yml, which uploads this directory as a build
      // artifact on every run (`if: always()`) so a future failure's full detail is recoverable even when the
      // console log again comes back summary-only.
      Tests.Argument(TestFrameworks.ScalaTest, "-u", "target/test-reports"),
      // Master CI's Test job has hung for 50+ minutes with no output and no way to tell which test was stuck: the
      // console reporter prints nothing per test, and nothing at all for a run that never ends. The slowpoke
      // detector raises an alert naming any test still running after 120 s, then every 60 s; HangReporter prints
      // those alerts straight to stdout together with the stacks of the threads running suites, the IO runtime's
      // compute and blocker threads, and any thread blocked, holding a lock or waiting on a class/lazy-val initialiser.
      // Each alert also probes the global IO runtime with a trivial IO and aborts the run, with the thread dump, when it
      // cannot answer; the reporter's construction installs the JVM-wide uncaught-exception handler (RuntimeWatch).
      Tests.Argument(TestFrameworks.ScalaTest, "-W", "120", "60"),
      Tests.Argument(TestFrameworks.ScalaTest, "-C", "com.serenity.testkit.HangReporter")
    ),
    // Real-OS-boundary specs (a genuine loopback socket, a genuine sun.misc.Signal.raise -- see
    // com.serenity.testkit.RealBoundaryTest's doc comment) are excluded from `sbt test`'s discovery of the whole
    // suite, but not from `testOnly`: this is scoped to the `test` task specifically (`Test / test / testOptions`),
    // one level more specific than the plain `Test / testOptions` above that `testOnly` still falls back to
    // unfiltered, so `realBoundaryTest` below can name these specs directly and still run them.
    Test / test / testOptions := (Test / testOptions).value ++ Seq(
      Tests.Argument(TestFrameworks.ScalaTest, "-l", "com.serenity.testkit.RealBoundaryTest")
    )
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
  "org.typelevel" %% "cats-effect" % "3.7.1",
  "co.fs2"        %% "fs2-core"    % "3.13.0",
  "co.fs2"        %% "fs2-io"      % "3.13.0",
  "org.scalatest" %% "scalatest"   % "3.2.19" % "test",
  // Drives IO programs on a mocked scheduler/clock for deterministic tests of timing-dependent code (timeouts,
  // intervals, idle cadence) -- see com.serenity.testkit.VirtualTime.
  "org.typelevel" %% "cats-effect-testkit" % "3.7.1" % "test",
  // Property and law testing, test scope only -- the assembled JAR is unchanged.
  // scalatestplus is pinned to the release matching ScalaTest 3.2.19 and brings ScalaCheck with it.
  // cats-laws tracks the cats-core 2.13.0 that cats-effect 3.7.1 already resolves, so no eviction.
  "org.scalatestplus"     %% "scalacheck-1-18"      % "3.2.19.0" % "test",
  "org.typelevel"         %% "cats-laws"            % "2.13.0"   % "test",
  "org.typelevel"         %% "discipline-scalatest" % "2.3.0"    % "test",
  // Independent oracle for the golden rich-document specs (Apache-2.0): every DOCX Serenity writes must open in XWPF.
  // Test scope only, so the assembled JAR and startup are unchanged.
  "org.apache.poi"         % "poi-ooxml"            % "5.4.1"    % "test",
  "com.github.pureconfig" %% "pureconfig-core"      % "0.17.10",
  // Launch-argument parsing. decline-effect is only for its cats-effect glue; the command itself is plain decline,
  // so `LaunchOptions.parse` stays a pure function over args that specs can call directly.
  "com.monovore" %% "decline"        % "2.6.2",
  "com.monovore" %% "decline-effect" % "2.6.2"
)

val circeVersion = "0.14.16"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core"    % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser"  % circeVersion
)

val log4CatsVersion = "2.8.0"

libraryDependencies ++= Seq(
  "org.typelevel"   %% "log4cats-core"   % log4CatsVersion,
  "org.typelevel"   %% "log4cats-slf4j"  % log4CatsVersion,
  "ch.qos.logback"   % "logback-classic" % "1.5.38",
  "net.java.dev.jna" % "jna-platform"    % "5.19.1"
)

val commonMarkVersion = "0.30.0"

libraryDependencies ++= Seq(
  "org.commonmark"    % "commonmark"                     % commonMarkVersion,
  "org.commonmark"    % "commonmark-ext-gfm-tables"      % commonMarkVersion,
  "org.commonmark"    % "commonmark-ext-task-list-items" % commonMarkVersion,
  "org.xhtmlrenderer" % "flying-saucer-core"             % "10.5.0"
)

// EPUBCheck (W3C, BSD-3-Clause, so compatible with this project's GPL) validates the exported EPUB in specs only.
// Test scope: it is never on the runtime classpath and never reaches the assembled JAR.
// xercesImpl is excluded because it takes over the JVM's default XML parser and then rejects the hardening flags
// RichTextXmlParser sets, which would break every DOCX/ODT spec sharing this test classpath.
libraryDependencies += ("org.w3c" % "epubcheck" % "5.4.0" % Test).exclude("xerces", "xercesImpl")

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

// FontBox reads the bundled fonts' hmtx/hhea tables, so the paginator measures the very files the PDF will embed and
// no AWT is involved (#1206, #2006). pdfbox-io and commons-logging come with it. PDFBox paints the PagedDocument into
// the PDF, embedding those same files.
libraryDependencies ++= Seq(
  "org.apache.pdfbox" % "fontbox" % "3.0.8",
  "org.apache.pdfbox" % "pdfbox"  % "3.0.8"
)
