// Copied into the build root only by .github/workflows/bench-artifacts.yml, never committed there, so it applies to
// any ref being benchmarked (including commits from before this file existed) and the normal build, tests and
// `Serenity.jar` never see it.
//
// `Test / assembly` packs the whole Test classpath (main and test classes, test-scope dependencies) into one jar, so
// the laptop runs `com.serenity.perf.*` with plain `java -cp serenity-perf.jar` and no sbt.
import sbtassembly.AssemblyPlugin.autoImport.*

inConfig(Test)(baseAssemblySettings)

Test / assembly / assemblyJarName := "serenity-perf.jar"
Test / assembly / mainClass       := None
// `baseAssemblySettings` resets the Test-scoped strategy to the plugin default; reuse the main jar's rules instead.
// The Test classpath also carries jing and epubcheck, which both bundle `com/thaiopensource` with differing
// `Messages.properties` and `Localizer.class`; either copy will do for the benchmarks.
Test / assembly / assemblyMergeStrategy := {
  val main = (assembly / assemblyMergeStrategy).value
  (path: String) =>
    path match {
      case PathList("com", "thaiopensource", _*) => MergeStrategy.first
      case other                                 => main(other)
    }
}
