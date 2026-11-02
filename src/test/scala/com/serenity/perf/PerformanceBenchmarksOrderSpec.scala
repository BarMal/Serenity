package com.serenity.perf

import java.awt.GraphicsEnvironment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The warm-up extension spends one budget in list order (the first rows are extended, the rest are not and still
  * benefit from what the first warmed), so the order of the suite is part of what a run measures: reordering it moves
  * which rows are warmed first, and has to be done on purpose.
  */
class PerformanceBenchmarksOrderSpec extends AnyFlatSpec with Matchers:

  private val firstRows = List(
    "rope.large_json.search",
    "rope.large_json.cursor_offset",
    "layout.large_multiline.visible_viewport",
    "render.full_frame.java2d",
    "render.long_measured_line.java2d",
    "render.cursor_only.scene_reuse.java2d_overlay",
    "render.diagnostics_and_comments.java2d",
    "render.hidpi_frame.java2d",
    "layout.aligned_prose.cached_snapshot",
    "layout.aligned_prose.moved_lines",
    "layout.heading_prose.uncached_snapshot",
    "render.cell_grid.frame_ascii",
    "render.cell_grid.frame_mixed",
    "render.cell_grid.frame_emoji",
    "render.cell_grid.frame_emoji_clusters",
    "layout.cell_grid.emoji_cluster_lines",
    "layout.cell_grid.ascii_lines",
    "layout.cell_grid.mixed_lines",
    "reducer.normal_editing",
    "reducer.backspace",
    "reducer.delete_word_backward",
    "reducer.arrow_navigation",
    "reducer.extend_selection",
    "reducer.multi_cursor_insert",
    "reducer.multi_cursor_move",
    "reducer.multi_cursor_move_down",
    "reducer.deep_scroll.plain",
    "reducer.deep_scroll.rich_text",
    "prose.rich_text_typing",
    "prose.markdown_typing.chapter_ghosts",
    "prose.markdown_typing.notes_pane",
    "prose.markdown_enter.renumber",
    "damage.single_char_long_line.rows",
    "damage.single_char_long_line.cells",
    "damage.single_char_short_line.rows",
    "damage.single_char_short_line.cells",
    "damage.multi_cursor.rows",
    "damage.multi_cursor.cells",
    "damage.markdown.rows",
    "damage.markdown.cells"
  )

  "The performance suite" should "start with the rows the warm-up budget is spent on, in this order" in {
    assume(!GraphicsEnvironment.isHeadless, "Building the suite needs a window, which needs a display")
    PerformanceBenchmarks.withSuite(_.map(_.name).take(firstRows.length)) shouldBe firstRows
  }

  it should "have no two benchmarks with the same name" in {
    assume(!GraphicsEnvironment.isHeadless, "Building the suite needs a window, which needs a display")
    val names = PerformanceBenchmarks.withSuite(_.map(_.name))
    names.distinct shouldBe names
  }
