package com.serenity.project

import java.nio.file.{Files, Path}

import scala.io.{Codec, Source}

import cats.effect.IO

/** Project-level workflow supported by Serenity's command runner. */
enum ProjectTaskKind(val label: String, val lowerLabel: String):
  case Build        extends ProjectTaskKind("Build", "build")
  case Test         extends ProjectTaskKind("Test", "test")
  case Run          extends ProjectTaskKind("Run", "run")
  case Debug        extends ProjectTaskKind("Debug", "debug")
  case Dependencies extends ProjectTaskKind("Dependencies", "dependency")

/** Shell command selected for a project workflow. */
case class ProjectTaskCommand(
    kind: ProjectTaskKind,
    ecosystemLabel: String,
    workingDirectory: Path,
    executable: String,
    arguments: List[String]
):
  def commandLine: List[String] =
    executable :: arguments

  def renderedCommand: String =
    commandLine.mkString(" ")

/** Completed project workflow process result. */
case class ProjectTaskResult(
    command: ProjectTaskCommand,
    exitCode: Int,
    output: String
)

/** Detects project roots and maps them to conventional build/test/run/debug/dependency commands. */
object ProjectTaskDetector:

  private case class Provider(
      ecosystemLabel: String,
      markers: List[String],
      commands: Map[ProjectTaskKind, List[String]]
  ):

    def commandFor(root: Path, kind: ProjectTaskKind): Option[ProjectTaskCommand] =
      commands.get(kind).collect {
        case executable :: arguments =>
          ProjectTaskCommand(kind, ecosystemLabel, root, executable, arguments)
      }

    def matches(root: Path): Boolean =
      markers.exists(marker => Files.exists(root.resolve(marker)))

  private val providers: List[Provider] =
    List(
      Provider(
        ecosystemLabel = "sbt",
        markers = List("build.sbt"),
        commands = Map(
          ProjectTaskKind.Build        -> List("sbt", "compile"),
          ProjectTaskKind.Test         -> List("sbt", "test"),
          ProjectTaskKind.Run          -> List("sbt", "run"),
          ProjectTaskKind.Debug        -> List("sbt", "-jvm-debug", "5005", "run"),
          ProjectTaskKind.Dependencies -> List("sbt", "update")
        )
      ),
      Provider(
        ecosystemLabel = "npm",
        markers = List("package.json"),
        commands = Map(
          ProjectTaskKind.Build        -> List("npm", "run", "build"),
          ProjectTaskKind.Test         -> List("npm", "test"),
          ProjectTaskKind.Run          -> List("npm", "run", "start"),
          ProjectTaskKind.Debug        -> List("npm", "run", "debug"),
          ProjectTaskKind.Dependencies -> List("npm", "ls", "--depth=0")
        )
      ),
      Provider(
        ecosystemLabel = "cargo",
        markers = List("Cargo.toml"),
        commands = Map(
          ProjectTaskKind.Build        -> List("cargo", "build"),
          ProjectTaskKind.Test         -> List("cargo", "test"),
          ProjectTaskKind.Run          -> List("cargo", "run"),
          ProjectTaskKind.Debug        -> List("cargo", "run"),
          ProjectTaskKind.Dependencies -> List("cargo", "tree")
        )
      ),
      Provider(
        ecosystemLabel = "maven",
        markers = List("pom.xml"),
        commands = Map(
          ProjectTaskKind.Build        -> List("mvn", "compile"),
          ProjectTaskKind.Test         -> List("mvn", "test"),
          ProjectTaskKind.Run          -> List("mvn", "exec:java"),
          ProjectTaskKind.Debug        -> List("mvnDebug", "test"),
          ProjectTaskKind.Dependencies -> List("mvn", "dependency:tree")
        )
      ),
      Provider(
        ecosystemLabel = "gradle",
        markers = List("build.gradle", "build.gradle.kts"),
        commands = Map(
          ProjectTaskKind.Build        -> List("gradle", "build"),
          ProjectTaskKind.Test         -> List("gradle", "test"),
          ProjectTaskKind.Run          -> List("gradle", "run"),
          ProjectTaskKind.Debug        -> List("gradle", "test", "--debug-jvm"),
          ProjectTaskKind.Dependencies -> List("gradle", "dependencies")
        )
      ),
      Provider(
        ecosystemLabel = "go",
        markers = List("go.mod"),
        commands = Map(
          ProjectTaskKind.Build        -> List("go", "build", "./..."),
          ProjectTaskKind.Test         -> List("go", "test", "./..."),
          ProjectTaskKind.Run          -> List("go", "run", "."),
          ProjectTaskKind.Debug        -> List("go", "test", "-c", "."),
          ProjectTaskKind.Dependencies -> List("go", "list", "-m", "all")
        )
      ),
      Provider(
        ecosystemLabel = "make",
        markers = List("Makefile"),
        commands = Map(
          ProjectTaskKind.Build        -> List("make"),
          ProjectTaskKind.Test         -> List("make", "test"),
          ProjectTaskKind.Run          -> List("make", "run"),
          ProjectTaskKind.Debug        -> List("make", "debug"),
          ProjectTaskKind.Dependencies -> List("make", "dependencies")
        )
      )
    )

  val supportedMarkers: List[String] =
    providers.flatMap(_.markers).distinct

  def detect(start: Path, kind: ProjectTaskKind): Option[ProjectTaskCommand] =
    startingDirectory(start).flatMap(dir => walkUp(dir, kind))

  private def startingDirectory(start: Path): Option[Path] =
    val absolute = start.toAbsolutePath.normalize()
    if Files.isDirectory(absolute) then Some(absolute)
    else Option(absolute.getParent)

  private def walkUp(directory: Path, kind: ProjectTaskKind): Option[ProjectTaskCommand] =
    providers
      .find(_.matches(directory))
      .flatMap(_.commandFor(directory, kind))
      .orElse(
        Option(directory.getParent)
          .filter(parent => parent != directory)
          .flatMap(parent => walkUp(parent, kind))
      )

/** Runs a detected project task and captures its combined output. */
object ProjectTaskRunner:

  private val MaxOutputLength = 20_000

  def run(command: ProjectTaskCommand): IO[ProjectTaskResult] =
    IO.blocking {
      val builder = ProcessBuilder(command.commandLine*)
      builder.directory(command.workingDirectory.toFile)
      builder.redirectErrorStream(true)

      val process = builder.start()
      val source  = Source.fromInputStream(process.getInputStream)(using Codec.UTF8)
      val output =
        try source.mkString
        finally source.close()
      val exitCode = process.waitFor()

      ProjectTaskResult(command, exitCode, trimOutput(output))
    }

  private def trimOutput(output: String): String =
    if output.length <= MaxOutputLength then output
    else output.takeRight(MaxOutputLength).prependedAll("[output trimmed]\n")

/** Formats project task status for the pinned terminal surface. */
object ProjectTaskTerminal:

  def noTask(kind: ProjectTaskKind, start: Path): String =
    s"""No ${kind.lowerLabel} task found.
       |
       |Start path:
       |${start.toAbsolutePath.normalize()}
       |
       |Supported project markers:
       |${ProjectTaskDetector.supportedMarkers.mkString(", ")}
       |""".stripMargin

  def started(command: ProjectTaskCommand): String =
    s"""Running ${command.kind.lowerLabel} task for ${command.ecosystemLabel}.
       |
       |Directory:
       |${command.workingDirectory}
       |
       |Command:
       |${command.renderedCommand}
       |""".stripMargin

  def completed(result: ProjectTaskResult): String =
    s"""${started(result.command)}
       |Exit code:
       |${result.exitCode}
       |
       |Output:
       |${if result.output.trim.isEmpty then "(no output)" else result.output}
       |""".stripMargin

  def failedToStart(command: ProjectTaskCommand, error: Throwable): String =
    s"""Failed to start ${command.kind.lowerLabel} task for ${command.ecosystemLabel}.
       |
       |Directory:
       |${command.workingDirectory}
       |
       |Command:
       |${command.renderedCommand}
       |
       |Error:
       |${error.getMessage}
       |""".stripMargin
