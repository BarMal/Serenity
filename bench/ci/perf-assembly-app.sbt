// Copied into the build root only by .github/workflows/bench-artifacts.yml, never committed there, and only for refs
// whose build has an `app` project (perf-assembly.sbt serves older refs, where the root project holds the sources).
//
// `app/Test / assembly` packs the whole Test classpath (main and test classes, test-scope dependencies) into one jar, so
// the laptop runs `com.serenity.perf.*` with plain `java -cp serenity-perf.jar` and no sbt.
import sbtassembly.AssemblyPlugin.autoImport.*

inScope(Scope(Select(LocalProject("app")), Zero, Zero, Zero))(inConfig(Test)(baseAssemblySettings))

LocalProject("app") / Test / assembly / assemblyJarName := "serenity-perf.jar"
LocalProject("app") / Test / assembly / mainClass       := None
// `baseAssemblySettings` resets the Test-scoped strategy to the plugin default; reuse the main jar's rules instead.
// The Test classpath also carries jing and epubcheck, which both bundle `com/thaiopensource` with differing
// `Messages.properties` and `Localizer.class`; either copy will do for the benchmarks.
LocalProject("app") / Test / assembly / assemblyMergeStrategy := {
  val main = (LocalProject("app") / assembly / assemblyMergeStrategy).value
  (path: String) =>
    path match {
      case PathList("com", "thaiopensource", _*) => MergeStrategy.first
      case other                                 => main(other)
    }
}
