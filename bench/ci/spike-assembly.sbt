// Copied into a spike checkout's build root only by .github/workflows/bench-artifacts.yml. The spike project already
// gets an `assembly` task from sbt-assembly; this names the jar and gives it the root project's merge rules, so the fat
// jar carries Skiko and its linux-x64 native runtime and runs with plain `java -cp serenity-spike.jar`.
import sbtassembly.AssemblyPlugin.autoImport.*
import sbtassembly.MergeStrategy

LocalProject("spike") / assembly / assemblyJarName := "serenity-spike.jar"
LocalProject("spike") / assembly / mainClass       := Some("com.serenity.spike.SkikoSpike")
// Kotlin, kotlinx-coroutines and the JetBrains annotations that Skiko brings each ship a different multi-release
// `META-INF/versions/9/module-info.class`; the jar runs from the classpath, so module descriptors are dead weight.
LocalProject("spike") / assembly / assemblyMergeStrategy := {
  val rootStrategy = (LocalProject("root") / assembly / assemblyMergeStrategy).value
  (path: String) => if (path.endsWith("module-info.class")) MergeStrategy.discard else rootStrategy(path)
}
