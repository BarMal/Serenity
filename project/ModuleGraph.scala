import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.collection.JavaConverters._

/** File and module dependency graph read from the compiler's semanticdb output (#2502).
  *
  * An edge `a -> b` means a symbol occurrence in `a` resolves to a global definition in `b`. That is what the compiler
  * needed, so it sees dependencies an import scan cannot: inherited members, extension methods, givens and inferred
  * types.
  *
  * Modules are assigned by path (see [[Layers]]) with no file moves. A module may depend only on modules below it, so
  * an edge to a higher layer is an "upward" edge; those are ratcheted against a checked-in baseline that may only
  * shrink.
  */
object ModuleGraph {

  final case class Document(uri: String, definitions: Set[String], references: Set[String])

  final case class Edge(fromFile: String, fromModule: String, toFile: String, toModule: String) {
    def key: String = s"$fromModule\t$fromFile\t$toModule\t$toFile"
  }

  final case class Baseline(keys: Set[String])

  object Baseline {
    val empty: Baseline = Baseline(Set.empty)
  }

  /** Bottom to top. The last layer, `app`, takes every file no earlier layer claims. */
  val Layers: Seq[(String, Seq[String])] = Seq(
    "core-text"    -> Seq("rope/", "text/", "io/AtomicFileWriter.scala", "io/RenameRetryPolicy.scala"),
    "lsp-protocol" -> Seq("lsp/config/", "lsp/model/", "lsp/client/"),
    "richtext"     -> Seq("richtext/"),
    "manuscript"   -> Seq("manuscript/", "exporting/", "publish/"),
    "theme"        -> Seq("ui/color/", "ui/theme/", "ui/display/"),
    "markdown"     -> Seq("markdown/"),
    "app"          -> Nil
  )

  /** Files assigned one by one because their package spans more than one layer. Checked before [[Layers]]. */
  private val Overrides: Map[String, String] = Map(
    "ui/theme/MarkdownInlineStyling.scala"  -> "markdown",
    "ui/theme/config/AppThemeManager.scala" -> "app",
    "publish/ManuscriptExport.scala"        -> "app"
  )

  private val ModuleOrder: Map[String, Int] = Layers.map(_._1).zipWithIndex.toMap
  private val Marker                        = "com/serenity/"

  def moduleOf(uri: String): String = {
    val at = uri.indexOf(Marker)
    if (at < 0) Layers.last._1
    else {
      val relative = uri.substring(at + Marker.length)
      Overrides
        .get(relative)
        .orElse(Layers.collectFirst { case (name, prefixes) if prefixes.exists(relative.startsWith) => name })
        .getOrElse(Layers.last._1)
    }
  }

  // -- semanticdb decoding ------------------------------------------------------------------------------------
  // Only the fields this graph needs, read straight off the protobuf wire format so the build gains no dependency:
  //   TextDocuments { documents = 1 }
  //   TextDocument  { uri = 2, occurrences = 6 }
  //   SymbolOccurrence { symbol = 2, role = 3 }   role: REFERENCE = 1, DEFINITION = 2

  final private class Reader(bytes: Array[Byte], var position: Int, end: Int) {
    def hasMore: Boolean = position < end

    def varint(): Long = {
      var result = 0L
      var shift  = 0
      var more   = true
      while (more) {
        val b = bytes(position).toInt & 0xff
        position += 1
        result |= (b & 0x7fL) << shift
        shift += 7
        more = (b & 0x80) != 0
      }
      result
    }

    def skip(wireType: Int): Unit = wireType match {
      case 0 => varint(); ()
      case 1 => position += 8
      case 2 =>
        val length = varint().toInt
        position += length
      case 5     => position += 4
      case other => throw new IllegalArgumentException(s"unsupported protobuf wire type $other")
    }

    def lengthDelimited(): Reader = {
      val length = varint().toInt
      val child  = new Reader(bytes, position, position + length)
      position += length
      child
    }

    def string(): String = {
      val length = varint().toInt
      val text   = new String(bytes, position, length, StandardCharsets.UTF_8)
      position += length
      text
    }

  }

  def parse(bytes: Array[Byte]): Seq[Document] = {
    val message = new Reader(bytes, 0, bytes.length)
    val out     = Vector.newBuilder[Document]
    while (message.hasMore) {
      val tag = message.varint()
      if ((tag >>> 3) == 1 && (tag & 7) == 2) out += parseDocument(message.lengthDelimited())
      else message.skip((tag & 7).toInt)
    }
    out.result()
  }

  private def parseDocument(document: Reader): Document = {
    var uri         = ""
    val definitions = Set.newBuilder[String]
    val references  = Set.newBuilder[String]
    while (document.hasMore) {
      val tag = document.varint()
      (tag >>> 3, (tag & 7).toInt) match {
        case (2, 2) => uri = document.string()
        case (6, 2) =>
          val (symbol, role) = parseOccurrence(document.lengthDelimited())
          if (role == 2) definitions += symbol else if (role == 1) references += symbol
        case (_, wire) => document.skip(wire)
      }
    }
    Document(uri, definitions.result(), references.result())
  }

  private def parseOccurrence(occurrence: Reader): (String, Long) = {
    var symbol = ""
    var role   = 0L
    while (occurrence.hasMore) {
      val tag = occurrence.varint()
      (tag >>> 3, (tag & 7).toInt) match {
        case (2, 2)    => symbol = occurrence.string()
        case (3, 0)    => role = occurrence.varint()
        case (_, wire) => occurrence.skip(wire)
      }
    }
    (symbol, role)
  }

  def readDocuments(root: File): Seq[Document] =
    if (!root.exists()) Nil
    else {
      val stream = Files.walk(root.toPath)
      try
        stream
          .iterator()
          .asScala
          .filter(_.toString.endsWith(".semanticdb"))
          .toVector
          .sorted
          .flatMap(path => parse(Files.readAllBytes(path)))
      finally stream.close()
    }

  // -- graphs -------------------------------------------------------------------------------------------------

  private def isGlobalDefinition(symbol: String): Boolean =
    symbol.nonEmpty && !symbol.startsWith("local") && !symbol.endsWith("/")

  /** Edges between the given documents. Package symbols (`com/serenity/lsp/`) are skipped: they are declared in many
    * files, so attributing one to a single file would invent edges.
    */
  def fileGraph(documents: Seq[Document]): Map[String, Set[String]] = {
    val owner: Map[String, String] =
      documents.flatMap(document => document.definitions.filter(isGlobalDefinition).map(_ -> document.uri)).toMap
    documents.map(document => document.uri -> document.references.flatMap(owner.get).filterNot(_ == document.uri)).toMap
  }

  /** Strongly connected components with more than one node, largest first (Tarjan). */
  def cycles[A : Ordering](graph: Map[A, Set[A]]): Seq[Set[A]] = {
    var counter = 0
    val index   = scala.collection.mutable.Map.empty[A, Int]
    val low     = scala.collection.mutable.Map.empty[A, Int]
    val stack   = scala.collection.mutable.Stack.empty[A]
    val onStack = scala.collection.mutable.Set.empty[A]
    val found   = Vector.newBuilder[Set[A]]

    def visit(node: A): Unit = {
      index(node) = counter
      low(node) = counter
      counter += 1
      stack.push(node)
      onStack += node
      graph.getOrElse(node, Set.empty[A]).toSeq.sorted.foreach { next =>
        if (!index.contains(next)) {
          visit(next)
          low(node) = math.min(low(node), low(next))
        } else if (onStack(next)) low(node) = math.min(low(node), index(next))
      }
      if (low(node) == index(node)) {
        val component = Set.newBuilder[A]
        var member    = stack.pop()
        onStack -= member
        component += member
        while (member != node) {
          member = stack.pop()
          onStack -= member
          component += member
        }
        found += component.result()
      }
    }

    graph.keys.toSeq.sorted.foreach(node => if (!index.contains(node)) visit(node))
    found.result().filter(_.size > 1).sortBy(c => (-c.size, c.min))
  }

  def moduleEdges(graph: Map[String, Set[String]]): Seq[Edge] =
    graph.toSeq.sortBy(_._1).flatMap {
      case (from, targets) =>
        targets.toSeq.sorted.map(to => Edge(from, moduleOf(from), to, moduleOf(to)))
    }

  def upwardEdges(graph: Map[String, Set[String]]): Seq[Edge] =
    moduleEdges(graph).filter(edge => ModuleOrder(edge.fromModule) < ModuleOrder(edge.toModule))

  // -- ratchet ------------------------------------------------------------------------------------------------

  private val BaselineHeader = Seq(
    "# Module-graph baseline. Generated by `sbt writeModuleGraphBaseline`.",
    "# Each line is an edge from a lower layer to a higher one (see ModuleGraph.Layers), which a layer must not have.",
    "# This list may shrink, never grow. Removing an entry is the point.",
    "# Columns: from-module <TAB> from-file <TAB> to-module <TAB> to-file"
  )

  def readBaseline(file: File): Baseline =
    if (!file.exists()) Baseline.empty
    else
      Baseline(
        Files
          .readAllLines(file.toPath, StandardCharsets.UTF_8)
          .asScala
          .filter(line => line.trim.nonEmpty && !line.trim.startsWith("#"))
          .toSet
      )

  def writeBaseline(file: File, edges: Seq[Edge]): Unit = {
    val lines = BaselineHeader ++ edges.map(_.key).distinct.sorted
    Files.write(file.toPath, lines.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
    ()
  }

  def check(documents: Seq[Document], baseline: Baseline): Option[String] = {
    val current = upwardEdges(fileGraph(documents))
    val keys    = current.map(_.key).toSet
    val added   = current.filterNot(edge => baseline.keys.contains(edge.key))
    val gone    = baseline.keys.diff(keys).toSeq.sorted

    val problems =
      (if (added.isEmpty) Nil
       else
         Seq(
           s"${added.size} new upward module edge(s) -- a layer may depend only on layers below it:",
           added.map(e => s"  ${e.fromModule} -> ${e.toModule}: ${e.fromFile} -> ${e.toFile}").mkString("\n")
         )) ++
        (if (gone.isEmpty) Nil
         else
           Seq(
             s"${gone.size} baseline entr(y/ies) no longer apply -- run `sbt writeModuleGraphBaseline` to bank the win:",
             gone.map(key => s"  ${key.replace('\t', ' ')}").mkString("\n")
           ))
    if (problems.isEmpty) None else Some(problems.mkString("\n"))
  }

  // -- report -------------------------------------------------------------------------------------------------

  private def packageOf(uri: String): String = {
    val at       = uri.indexOf(Marker)
    val relative = if (at < 0) uri else uri.substring(at + Marker.length)
    relative.split('/').dropRight(1).mkString("/")
  }

  def report(documents: Seq[Document]): String = {
    val graph      = fileGraph(documents)
    val edges      = moduleEdges(graph)
    val components = cycles(graph)
    val upward     = upwardEdges(graph)
    val perModule  = graph.keys.toSeq.groupBy(moduleOf).mapValues(_.size)

    val moduleLines = Layers.map {
      case (name, _) =>
        val outgoing = edges.filter(e => e.fromModule == name && e.toModule != name)
        val targets  = outgoing.groupBy(_.toModule).mapValues(_.size).toSeq.sortBy { case (m, _) => ModuleOrder(m) }
        f"  $name%-13s ${perModule.getOrElse(name, 0)}%5d files  -> ${if (targets.isEmpty) "(nothing outside itself)" else targets.map { case (m, n) => s"$m $n" }.mkString(", ")}"
    }

    val moduleCycles = cycles(
      edges.filter(e => e.fromModule != e.toModule).groupBy(_.fromModule).mapValues(_.map(_.toModule).toSet).toMap
    )

    val componentLines = components.map { component =>
      val byPackage = component.toSeq.groupBy(packageOf).mapValues(_.size).toSeq.sortBy { case (p, n) => (-n, p) }
      s"  ${component.size} files: " + byPackage.take(8).map { case (p, n) => s"$p $n" }.mkString(", ")
    }

    (Seq(
      s"module graph: ${graph.size} files, ${graph.values.map(_.size).sum} file edges, ${upward.size} upward module edges",
      "modules (bottom to top), with edges to other modules:"
    ) ++ moduleLines ++ Seq(
      s"module-level cycles: ${if (moduleCycles.isEmpty) "none" else moduleCycles.map(_.toSeq.sorted.mkString("{", ", ", "}")).mkString(", ")}",
      s"file-level cycles (${components.size}), largest first:"
    ) ++ componentLines).mkString("\n") + "\n"
  }

}
