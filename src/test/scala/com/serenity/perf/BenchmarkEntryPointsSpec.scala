package com.serenity.perf

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rules that keep a benchmark runnable from `serenity-perf.jar` on a laptop (#2486), checked on the sources: a
  * `main` that forgot the wrapper, or a read of the source tree, only shows when someone runs it there.
  */
class BenchmarkEntryPointsSpec extends AnyFlatSpec with Matchers:

  private val perfRoot = Paths.get("src", "test", "scala", "com", "serenity", "perf")

  private val MainHeader     = "def main(args: Array[String]): Unit ="
  private val WrapperLine    = "BenchmarkMain.run:"
  private val SourceTreeRead = """Paths?\.(get|of)\(\s*"src[/"]""".r

  private def perfSources: List[(String, List[String])] =
    Using.resource(Files.walk(perfRoot, 1)) { stream =>
      stream
        .iterator()
        .asScala
        .filter(path => Files.isRegularFile(path) && path.toString.endsWith(".scala"))
        .map(path => path.getFileName.toString -> Files.readAllLines(path).asScala.toList)
        .toList
        .sortBy(_._1)
    }

  private def isSpec(fileName: String): Boolean = fileName.endsWith("Spec.scala")

  "Every main in the perf directory" should "run its body inside BenchmarkMain" in {
    val unwrapped = perfSources.filterNot((name, _) => isSpec(name)).flatMap { (name, lines) =>
      lines.zipWithIndex.collect {
        case (line, index) if line.trim == MainHeader && !lines.lift(index + 1).exists(_.trim == WrapperLine) =>
          s"$name:${index + 1}"
      }
    }

    withClue("a main whose body is not inside BenchmarkMain.run, so a failure can leave the JVM running: ")(
      unwrapped shouldBe Nil
    )
  }

  "A benchmark outside a spec" should "not read the source tree by a relative path" in {
    val readers = perfSources.filterNot((name, _) => isSpec(name)).flatMap { (name, lines) =>
      lines.zipWithIndex.collect {
        case (line, index) if SourceTreeRead.findFirstIn(line).nonEmpty => s"$name:${index + 1}"
      }
    }

    withClue("reads under src/ that serenity-perf.jar has no tree to satisfy; bundle the input as a resource: ")(
      readers shouldBe Nil
    )
  }
