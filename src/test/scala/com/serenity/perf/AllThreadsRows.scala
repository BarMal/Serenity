package com.serenity.perf

/** The rows whose work runs off the benchmark's own thread, so `allocation_p50_bytes` sees only the caller's share. An
  * entry covers that name and `<name>.<variant>`. It is listed here, not at each declaration, so the flag is one place
  * to read and one place the suite checks against the names it declares.
  */
private[perf] object AllThreadsRows:

  private val Listed: List[String] =
    List(
      "laptop.input.state_manager.move_down_up",
      "laptop.input.state_manager.type_and_delete",
      "laptop.input.state_manager.continuous_typing",
      "laptop.input.state_manager.page_down_up",
      "laptop.input.state_manager.typing_random_letters",
      "laptop.input.state_manager.typing_long_paragraph",
      "laptop.input.state_manager.typewriter_keystroke",
      "laptop.input.state_manager.go_to_line_3000_paragraphs",
      "laptop.input.state_manager.two_runtimes",
      "laptop.present.swing_fresh_frame_synced_1500x1000",
      "laptop.present.swing_paint_caret_1500x1000",
      "laptop.render.typing_frame_2x.ligature_font.skia_raster",
      "index.walk"
    )

  private def covers(name: String): Boolean = Listed.exists(entry => name == entry || name.startsWith(entry + "."))

  def apply(rows: List[BenchmarkRunner.Benchmark]): List[BenchmarkRunner.Benchmark] =
    rows.map(row => if covers(row.name) then row.measuringAllThreads else row)

  /** Entries that no declared name falls under: a renamed or removed row. */
  def unknownIn(names: List[String]): List[String] =
    Listed.filterNot(entry => names.exists(name => name == entry || name.startsWith(entry + ".")))
