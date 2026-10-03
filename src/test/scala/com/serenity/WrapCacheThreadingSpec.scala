package com.serenity

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Production code must measure wrapped lines through the shared `WrappedLineCache` its owner holds
  * (`RenderCaches.wrappedLines`, which `AuthoritativeUiScene` shares). `WrappedLineCache.Uncached` stays only as the
  * default of a `wrapCache` parameter, so a spec can call a measuring function without building a cache; every
  * `src/main` call to such a function has to pass its cache explicitly, or that path silently stops sharing wraps and
  * loses its visual-row index.
  */
class WrapCacheThreadingSpec extends AnyFlatSpec with Matchers:
  import WrapCacheThreadingSpec.*

  private val mainRoot = Paths.get("src", "main", "scala")

  "src/main" should "pass the shared wrap cache to every function that would otherwise default to Uncached" in {
    val sources = scalaSources(mainRoot)
    declarationsWithUncachedDefault(sources) should not be empty
    violations(sources) shouldBe Nil
  }

  "The wrap-cache threading check" should "flag a call that leaves the cache to its default" in {
    val sources = Map(
      "a/Measure.scala" ->
        """object Measure:
          |  def rows(text: String, wrapCache: WrappedLineCache = WrappedLineCache.Uncached): Int = 1
          |""".stripMargin,
      "a/Caller.scala" ->
        """object Caller:
          |  def threaded(cache: WrappedLineCache) = Measure.rows("x", wrapCache = cache)
          |  def positional(wrapCache: WrappedLineCache) = Measure.rows("x", wrapCache)
          |  // Measure.rows("commented out")
          |  def otherObject = Elsewhere.rows("not Measure's")
          |  def forgotten = Measure.rows(
          |    "y"
          |  )
          |""".stripMargin
    )
    violations(sources) shouldBe List("a/Caller.scala:6 calls Measure.rows without passing its wrap cache")
  }

  it should "flag a class constructed without its cache and a stray use of Uncached" in {
    val sources = Map(
      "a/Component.scala" ->
        """class Component(id: Int, wrapCache: WrappedLineCache = WrappedLineCache.Uncached)
          |""".stripMargin,
      "a/Owner.scala" ->
        """object Owner:
          |  val pooled  = new Component(1)
          |  val shared  = Component(2, wrapCache = scene.wrappedLines)
          |  val bypass  = WrappedLineCache.Uncached
          |""".stripMargin
    )
    violations(sources) shouldBe List(
      "a/Owner.scala:2 calls Component without passing its wrap cache",
      "a/Owner.scala:4 uses WrappedLineCache.Uncached outside a parameter default"
    )
  }

  private def scalaSources(root: Path): Map[String, String] =
    Using.resource(Files.walk(root)) { stream =>
      stream
        .iterator()
        .asScala
        .filter(path => Files.isRegularFile(path) && path.toString.endsWith(".scala"))
        .map(path => root.relativize(path).toString.replace('\\', '/') -> Files.readString(path))
        .toMap
    }

object WrapCacheThreadingSpec:

  /** Where `WrappedLineCache.Uncached` is defined, and the only file allowed to refer to it beyond parameter defaults.
    */
  private val DefiningFile = "com/serenity/ui/layout/WrappedLineCache.scala"

  private val UncachedDefault = """:\s*WrappedLineCache\s*=\s*WrappedLineCache\s*\.\s*Uncached""".r
  private val Uncached        = """WrappedLineCache\s*\.\s*Uncached""".r
  private val Declaration     = """\b(def|class)\s+(\w+)""".r
  private val TopLevelObject  = """(?m)^(?:(?:private|protected)(?:\[\w+\])?\s+|final\s+)*object\s+(\w+)""".r
  private val Export          = """export\s+(\w+)\s*\.\s*\{([^}]*)\}""".r
  private val CacheArgument   = """(?s)\s*(?:wrapCache\s*=.*|[\w.]*(?:wrapCache|wrappedLines)\s*)""".r

  final private case class Declared(file: String, name: String, isClass: Boolean, owner: Option[String])

  private def declarationsWithUncachedDefault(sources: Map[String, String]): List[(String, String)] =
    declared(sources.view.mapValues(stripCommentsAndStrings).toMap).map(d => d.file -> d.name)

  private def declared(code: Map[String, String]): List[Declared] =
    code.toList.sortBy(_._1).flatMap { (file, text) =>
      UncachedDefault
        .findAllMatchIn(text)
        .flatMap { default =>
          Declaration.findAllMatchIn(text.substring(0, default.start)).toList.lastOption.map { declaration =>
            val owner = TopLevelObject.findAllMatchIn(text.substring(0, declaration.start)).toList.lastOption
            Declared(file, declaration.group(2), declaration.group(1) == "class", owner.map(_.group(1)))
          }
        }
        .distinct
    }

  /** Each declaring object, plus every object that re-exports one of its members, by member name. */
  private def ownersByName(code: Map[String, String], declarations: List[Declared]): Map[String, Set[String]] =
    val exported = code.values.toList.flatMap { text =>
      Export.findAllMatchIn(text).flatMap { clause =>
        val exporter = TopLevelObject.findAllMatchIn(text.substring(0, clause.start)).toList.lastOption.map(_.group(1))
        clause.group(2).split(',').map(_.trim).filter(_.nonEmpty).flatMap(name => exporter.map(name -> _))
      }
    }
    declarations.groupMapReduce(_.name)(_.owner.toSet)(_ ++ _).map { (name, owners) =>
      name -> (owners ++ exported.collect { case (`name`, exporter) => exporter })
    }

  def violations(sources: Map[String, String]): List[String] =
    val code         = sources.view.mapValues(stripCommentsAndStrings).toMap
    val declarations = declared(code)
    val owners       = ownersByName(code, declarations)
    val calls = code.toList.sortBy(_._1).flatMap { (file, text) =>
      declarations.distinctBy(d => (d.name, d.isClass)).flatMap { declaration =>
        callSites(text, declaration.name).collect {
          case (offset, qualifier, arguments)
              if isCallTo(declaration, owners.getOrElse(declaration.name, Set.empty), qualifier, file) &&
                !splitArguments(arguments).exists(CacheArgument.matches) =>
            val target = qualifier.fold(declaration.name)(q => s"$q.${declaration.name}")
            (file, lineOf(text, offset), s"calls $target without passing its wrap cache")
        }
      }
    }
    val strays = code.toList.sortBy(_._1).filterNot(_._1.endsWith(DefiningFile)).flatMap { (file, text) =>
      val defaults = UncachedDefault.findAllMatchIn(text).map(m => m.end).toSet
      Uncached
        .findAllMatchIn(text)
        .filterNot(m => defaults.contains(m.end))
        .map(m => (file, lineOf(text, m.start), "uses WrappedLineCache.Uncached outside a parameter default"))
    }
    (calls ++ strays).sortBy((file, line, _) => (file, line)).map((file, line, what) => s"$file:$line $what")

  private def isCallTo(declaration: Declared, owners: Set[String], qualifier: Option[String], file: String): Boolean =
    qualifier match
      case Some(name) => owners.contains(name)
      case None       => declaration.isClass || declaration.file == file

  /** Every `name(` that is not its own declaration: where it starts, what qualifies it, and its first argument list. */
  private def callSites(text: String, name: String): List[(Int, Option[String], String)] =
    s"""\\b$name\\s*\\(""".r.findAllMatchIn(text).toList.flatMap { call =>
      val before        = text.substring(0, call.start)
      val isDeclaration = """(?:def|class)\s+$""".r.findFirstIn(before).nonEmpty
      val qualifier     = """(\w+)\s*\.\s*$""".r.findFirstMatchIn(before).map(_.group(1))
      Option.unless(isDeclaration)((call.start, qualifier, argumentsFrom(text, call.end)))
    }

  private def argumentsFrom(text: String, from: Int): String =
    @annotation.tailrec
    def close(index: Int, depth: Int): Int =
      if index >= text.length then index
      else
        text.charAt(index) match
          case '(' | '[' | '{' => close(index + 1, depth + 1)
          case ')' | ']' | '}' => if depth == 0 then index else close(index + 1, depth - 1)
          case _               => close(index + 1, depth)
    text.substring(from, close(from, 0))

  private def splitArguments(arguments: String): List[String] =
    arguments
      .foldLeft((List(""), 0)) {
        case ((current :: done, depth), ',') if depth == 0 => ("" :: current :: done, depth)
        case ((current :: done, depth), char) =>
          val nextDepth = char match
            case '(' | '[' | '{' => depth + 1
            case ')' | ']' | '}' => depth - 1
            case _               => depth
          ((current + char) :: done, nextDepth)
        case ((Nil, depth), char) => (List(char.toString), depth)
      }
      ._1
      .reverse

  private def lineOf(text: String, offset: Int): Int = text.substring(0, offset).count(_ == '\n') + 1

  /** Blanks comments and string literals, keeping every newline so offsets still map to source lines. */
  def stripCommentsAndStrings(source: String): String =
    val out = new StringBuilder(source.length)
    @annotation.tailrec
    def code(i: Int): Unit =
      if i < source.length then
        if source.startsWith("//", i) then lineComment(i)
        else if source.startsWith("/*", i) then blockComment(i + 2, 1, blank(i, 2))
        else if source.startsWith("\"\"\"", i) then tripleString(blank(i, 3))
        else if source.charAt(i) == '"' then string(blank(i, 1))
        else if source.charAt(i) == '\'' && i + 2 < source.length && source.charAt(i + 2) == '\'' then code(blank(i, 3))
        else if source.startsWith("'\\", i) && i + 3 < source.length && source.charAt(i + 3) == '\'' then
          code(blank(i, 4))
        else
          out.append(source.charAt(i))
          code(i + 1)
    def blank(i: Int, n: Int): Int =
      (i until math.min(source.length, i + n)).foreach(j => out.append(if source.charAt(j) == '\n' then '\n' else ' '))
      i + n
    def lineComment(i: Int): Unit =
      val end = source.indexOf('\n', i)
      code(blank(i, (if end < 0 then source.length else end) - i))
    @annotation.tailrec
    def blockComment(i: Int, depth: Int, unused: Int): Unit =
      if i >= source.length then ()
      else if source.startsWith("*/", i) then
        if depth == 1 then code(blank(i, 2)) else blockComment(blank(i, 2), depth - 1, 0)
      else if source.startsWith("/*", i) then blockComment(blank(i, 2), depth + 1, 0)
      else blockComment(blank(i, 1), depth, 0)
    @annotation.tailrec
    def string(i: Int): Unit =
      if i >= source.length then ()
      else if source.charAt(i) == '\\' then string(blank(i, 2))
      else if source.charAt(i) == '"' then code(blank(i, 1))
      else string(blank(i, 1))
    @annotation.tailrec
    def tripleString(i: Int): Unit =
      if i >= source.length then ()
      else if source.startsWith("\"\"\"", i) then code(blank(i, 3))
      else tripleString(blank(i, 1))
    code(0)
    out.toString
