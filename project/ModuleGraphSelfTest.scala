import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

/** Regression guard for [[ModuleGraph]] (#2502): the semanticdb decoder, the file graph, the strongly connected
  * components and the layering ratchet. Plain `require` assertions, as in `ArchitectureChecks.selfTest` -- `project/`
  * sources have no test dependency of their own.
  */
object ModuleGraphSelfTest {

  private def varint(out: ByteArrayOutputStream, value: Long): Unit = {
    var rest = value
    while (rest >= 0x80) {
      out.write(((rest & 0x7f) | 0x80).toInt)
      rest = rest >>> 7
    }
    out.write(rest.toInt)
  }

  private def lengthDelimited(field: Int, payload: Array[Byte]): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    varint(out, (field.toLong << 3) | 2)
    varint(out, payload.length.toLong)
    out.write(payload)
    out.toByteArray
  }

  private def varintField(field: Int, value: Long): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    varint(out, field.toLong << 3)
    varint(out, value)
    out.toByteArray
  }

  private def utf8(text: String): Array[Byte] = text.getBytes(StandardCharsets.UTF_8)

  private def occurrence(symbol: String, role: Int): Array[Byte] =
    lengthDelimited(2, utf8(symbol)) ++ varintField(3, role.toLong)

  private def encode(uri: String, occurrences: (String, Int)*): Array[Byte] = {
    val document =
      lengthDelimited(2, utf8(uri)) ++
        occurrences.flatMap { case (symbol, role) => lengthDelimited(6, occurrence(symbol, role)) }
    lengthDelimited(1, document)
  }

  private val Definition = 2
  private val Reference  = 1

  private def doc(uri: String, defines: Seq[String], references: Seq[String]): ModuleGraph.Document =
    ModuleGraph.Document(uri, defines.toSet, references.toSet)

  private def main(path: String): String = s"src/main/scala/com/serenity/$path"

  def run(): Unit = {
    decodesSemanticdb()
    buildsFileGraph()
    findsCycles()
    assignsModules()
    ratchetsUpwardEdges()
  }

  private def decodesSemanticdb(): Unit = {
    val unknownField = lengthDelimited(5, utf8("skipped symbol information")) ++ varintField(11, 300L)
    val bytes = lengthDelimited(
      1,
      lengthDelimited(2, utf8("src/main/scala/com/serenity/other/Skip.scala")) ++ unknownField
    ) ++ encode(
      "src/main/scala/com/serenity/rope/Rope.scala",
      "com/serenity/rope/Rope#"    -> Definition,
      "com/serenity/text/Newline#" -> Reference,
      "local7"                     -> Definition
    ) ++ encode("src/main/scala/com/serenity/text/Newline.scala", "com/serenity/text/Newline#" -> Definition)
    val parsed = ModuleGraph.parse(bytes)
    require(
      parsed.map(_.uri) == Seq(
        "src/main/scala/com/serenity/other/Skip.scala",
        "src/main/scala/com/serenity/rope/Rope.scala",
        "src/main/scala/com/serenity/text/Newline.scala"
      ),
      s"decoder must read every document in a TextDocuments message, got ${parsed.map(_.uri)}"
    )
    val rope = parsed(1)
    require(
      rope.definitions == Set("com/serenity/rope/Rope#", "local7"),
      "role DEFINITION (2) occurrences are definitions"
    )
    require(
      rope.references == Set("com/serenity/text/Newline#"),
      "role REFERENCE (1) occurrences are references"
    )
  }

  private def buildsFileGraph(): Unit = {
    val docs = Seq(
      doc(
        main("rope/Rope.scala"),
        Seq("com/serenity/rope/Rope#"),
        Seq("com/serenity/text/Newline#", "local1", "com/serenity/rope/")
      ),
      doc(main("text/Newline.scala"), Seq("com/serenity/text/Newline#", "com/serenity/text/"), Nil),
      doc(main("text/Self.scala"), Seq("com/serenity/text/Self#"), Seq("com/serenity/text/Self#"))
    )
    val graph = ModuleGraph.fileGraph(docs)
    require(
      graph(main("rope/Rope.scala")) == Set(main("text/Newline.scala")),
      "a reference to another file's definition is an edge; locals and package symbols are not"
    )
    require(graph(main("text/Self.scala")).isEmpty, "a file referencing its own definition is not an edge")
    require(graph.keySet == docs.map(_.uri).toSet, "every document is a node, even with no edges")
  }

  private def findsCycles(): Unit = {
    val graph = Map(
      "a" -> Set("b"),
      "b" -> Set("c"),
      "c" -> Set("a", "d"),
      "d" -> Set.empty[String],
      "e" -> Set("e")
    )
    val components = ModuleGraph.cycles(graph)
    require(components == Seq(Set("a", "b", "c")), s"only multi-node cycles are reported, got $components")
    val sizes = ModuleGraph.cycles(graph ++ Map("x" -> Set("y"), "y" -> Set("x"))).map(_.size)
    require(sizes == Seq(3, 2), s"largest cycle first, got $sizes")
  }

  private def assignsModules(): Unit = {
    val expected = Seq(
      "src/main/scala/com/serenity/rope/btree/BTreeEdit.scala"           -> "core-text",
      "src/main/scala/com/serenity/text/bidi/Run.scala"                  -> "core-text",
      "src/main/scala/com/serenity/io/AtomicFileWriter.scala"            -> "core-text",
      "src/main/scala/com/serenity/io/FileManager.scala"                 -> "app",
      "src/main/scala/com/serenity/lsp/model/Diagnostic.scala"           -> "lsp-protocol",
      "src/main/scala/com/serenity/lsp/client/LspFramer.scala"           -> "lsp-protocol",
      "src/main/scala/com/serenity/lsp/LspManager.scala"                 -> "app",
      "src/main/scala/com/serenity/richtext/XmlDom.scala"                -> "richtext",
      "src/main/scala/com/serenity/exporting/PdfExporter.scala"          -> "manuscript",
      "src/main/scala/com/serenity/ui/theme/Theme.scala"                 -> "theme",
      "src/main/scala/com/serenity/ui/fonts/FontLoader.scala"            -> "theme",
      "src/main/scala/com/serenity/markdown/BlockScan.scala"             -> "markdown",
      "src/main/scala/com/serenity/state/models/AppState.scala"          -> "app",
      "src/main/scala/Main.scala"                                        -> "app",
      "target/scala-3.9.0/src_managed/main/com/serenity/BuildInfo.scala" -> "app"
    )
    expected.foreach {
      case (uri, module) =>
        require(ModuleGraph.moduleOf(uri) == module, s"$uri belongs to $module, got ${ModuleGraph.moduleOf(uri)}")
    }
  }

  private def ratchetsUpwardEdges(): Unit = {
    val rope  = main("rope/Rope.scala")
    val state = main("state/models/AppState.scala")
    val lsp   = main("lsp/model/Diagnostic.scala")
    val clean = Seq(
      doc(rope, Seq("com/serenity/rope/Rope#"), Nil),
      doc(lsp, Seq("com/serenity/lsp/model/Diagnostic#"), Seq("com/serenity/rope/Rope#")),
      doc(
        state,
        Seq("com/serenity/state/models/AppState#"),
        Seq("com/serenity/rope/Rope#", "com/serenity/lsp/model/Diagnostic#")
      )
    )
    require(
      ModuleGraph.upwardEdges(ModuleGraph.fileGraph(clean)).isEmpty,
      "edges pointing down the layering are allowed"
    )

    val dirty  = doc(rope, Seq("com/serenity/rope/Rope#"), Seq("com/serenity/state/models/AppState#")) +: clean.tail
    val upward = ModuleGraph.upwardEdges(ModuleGraph.fileGraph(dirty))
    require(
      upward == Seq(ModuleGraph.Edge(rope, "core-text", state, "app")),
      s"core-text referencing app must be flagged, got $upward"
    )

    val empty = ModuleGraph.Baseline.empty
    require(ModuleGraph.check(clean, empty).isEmpty, "a clean graph passes an empty baseline")

    val failure = ModuleGraph.check(dirty, empty)
    require(
      failure.exists(report => report.contains(rope) && report.contains(state)),
      "an upward edge that is not in the baseline must fail the check and name both files"
    )

    val accepted = ModuleGraph.Baseline(upward.map(_.key).toSet)
    require(ModuleGraph.check(dirty, accepted).isEmpty, "a baselined upward edge passes")

    val stale = ModuleGraph.check(clean, accepted)
    require(
      stale.exists(_.contains("no longer")),
      "a baseline entry whose edge is gone must fail until it is removed, so the baseline can only shrink"
    )

    val file = java.io.File.createTempFile("module-graph-baseline", ".tsv")
    try {
      ModuleGraph.writeBaseline(file, upward)
      require(ModuleGraph.readBaseline(file) == accepted, "the baseline file round-trips")
    } finally file.delete()
  }

}
