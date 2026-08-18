import sbtassembly.AssemblyPlugin.autoImport.*
import sbtassembly.MergeStrategy

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.4"

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

Compile / scalacOptions ++= Seq(
  "-Wunused:all",
  "-Wvalue-discard",
  "-Wnonunit-statement"
  // NOT YET: "-Werror".
  //
  // Exhaustivity on a sealed hierarchy is only a warning, so without -Werror, sealing an ADT still
  // lets a missing case compile -- which makes it the natural completion of the sealing work.
  // (-Xfatal-warnings is the historical alias and is deprecated as of Scala 3.8; -Werror is the
  // spelling to use.)
  //
  // It cannot be switched on yet. Compiling this tree produces 150 warnings that -Werror would turn
  // into build failures:
  //   105  staged WartRemover findings (Null / Throw / OptionPartial / IterableOps, see below)
  //    36  unused symbols            [E198]
  //     4  non-exhaustive matches    [E029]
  //     2  potential-issue warnings  [E175]
  //     1  infix-method deprecation
  //
  // -Wconf cannot carve these out: under -Werror both the `w` and `i` actions still fail the build,
  // and `s` would hide the staged wart findings entirely, defeating the point of staging them.
  // So -Werror lands only once the backlog is genuinely cleared -- unused symbols are auto-fixable
  // with `sbt "Compile / scalafix RemoveUnused"`, and the 4 exhaustivity warnings need real fixes.
  // Kept out of this change so it stays a build change rather than a build-plus-42-code-fixes change.
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
    // Errors are the enforced set. Anything needing a migration first stays a warning until the
    // violations are cleared, then moves up. The plugin is CrossVersion.full, so a Scala upgrade needs
    // a matching WartRemover release -- 3.6.1 publishes for 3.8.4.
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
      Wart.IsInstanceOf
    ),
    // Reported but not failing. Each needs its own migration and is tracked separately; null and throw
    // are concentrated in Swing/AWT interop and the richtext/LSP parsers, which
    // docs/coding-standards.md already treats as the outermost boundary.
    //
    // Deliberately absent: Wart.MutableDataStructures. It flags StringBuilder, which
    // docs/coding-standards.md explicitly permits ("Private local mutation is acceptable when it is
    // contained and earns its place, for example StringBuilder during rendering or rope traversal").
    // 31 of its 33 hits here are exactly that sanctioned use, so the wart would fight the standard
    // rather than enforce it. Contained mutation is already governed by DisableSyntax.noVars.
    Compile / wartremoverWarnings ++= Seq(
      Wart.Null,
      Wart.Throw,
      Wart.OptionPartial,
      Wart.IterableOps
    ),
    // Tests are exempt, the same way Test / scalacOptions already drops the -W flags above. Test code
    // legitimately uses casts, throws and partial access to build failure fixtures and to assert on
    // representation invariants -- RopeSpec, for instance, subclasses Leaf to prove that search never
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
    )
  )

libraryDependencies ++= Seq(
  "org.typelevel"         %% "cats-effect"     % "3.7.0",
  "co.fs2"                %% "fs2-core"        % "3.13.0",
  "co.fs2"                %% "fs2-io"          % "3.13.0",
  "org.scalatest"         %% "scalatest"       % "3.2.19" % "test",
  "com.github.pureconfig" %% "pureconfig-core" % "0.17.9"
)

val circeVersion = "0.14.10"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core"    % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser"  % circeVersion
)

val log4CatsVersion = "2.7.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "log4cats-core"   % log4CatsVersion,
  "org.typelevel" %% "log4cats-slf4j"  % log4CatsVersion,
  "ch.qos.logback" % "logback-classic" % "1.5.18",
  "net.java.dev.jna" % "jna-platform"  % "5.12.0"
)

val commonMarkVersion = "0.27.0"

libraryDependencies ++= Seq(
  "org.commonmark"   % "commonmark"                   % commonMarkVersion,
  "org.commonmark"   % "commonmark-ext-gfm-tables"    % commonMarkVersion,
  "org.commonmark"   % "commonmark-ext-task-list-items" % commonMarkVersion,
  "org.xhtmlrenderer" % "flying-saucer-core"          % "10.2.2"
)
