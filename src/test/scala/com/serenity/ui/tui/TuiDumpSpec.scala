package com.serenity.ui.tui

import cats.effect.IO
import cats.syntax.all.*

import TuiScenarios.*

class TuiDumpSpec extends TuiSpec:

  private def dump(label: String): TuiScript[Unit] =
    screen.flatMapF(current =>
      IO {
        println(s"===== $label =====")
        current.paintedRows.foreach((row, line) => println(f"$row%3d|${line.replace('│', '|')}"))
        println(s"caret=${current.caret} visible=${current.caretVisible}")
      }
    )

  "dump" should "show what remains after escape" in runTui(TuiEnvironment.withFile("covered content")) {
    for
      _      <- openCommandPalette
      _      <- escape
      after  <- settledScreen
      _ <- liftIO(IO {
        println("ROWS-WITH-SEARCH=" + after.rowsContaining("search:"))
        after.rowsContaining("search:").foreach(r => println(s"  row $r = [${after.rowText(r).strip}]"))
        println("PAINTED=" + after.paintedRows.map((r, t) => s"$r:${t.strip.take(40)}"))
      })
    yield ()
  }

  it should "show what an arrow-down repaints" in runTui() {
    for
      _      <- searchCommands("line")
      before <- screen
      _      <- arrowDown
      after  <- screen
      _ <- liftIO(IO {
        val changed = after.changedRows(before).toList.sorted
        println(s"CHANGED-ROWS=$changed")
        changed.take(6).foreach { row =>
          println(s"  row $row before=[${before.rowText(row).strip.take(50)}] after=[${after.rowText(row).strip.take(50)}]")
          val cols = (0 until 200).filter(col => before.cellAt(col, row) != after.cellAt(col, row))
          println(s"    changed cols ${cols.take(6)} .. ${cols.size} total")
          cols.headOption.foreach(col =>
            println(s"    first: before=${before.cellAt(col, row)} after=${after.cellAt(col, row)}")
          )
        }
      })
    yield ()
  }

  it should "show what escape leaves behind" in runTui(TuiEnvironment.withFile("covered content")) {
    for
      before <- settledScreen
      _      <- openCommandPalette
      _      <- escape
      after  <- settledScreen
      _ <- liftIO(IO {
        val changed = after.changedCells(before).toList.sortBy(cell => (cell._2, cell._1))
        println(s"ESCAPE-CHANGED=${changed.size} sample=${changed.take(5)}")
        changed.take(3).foreach((col, row) =>
          println(s"  ($col,$row) before=${before.cellAt(col, row)} after=${after.cellAt(col, row)}")
        )
      })
    yield ()
  }

  it should "show a query matching nothing" in runTui() {
    for
      _ <- searchCommands("zzzznotacommand")
      _ <- dump("no matches")
      _ <- verifyState("surface")(st => println("SURFACES=" + st.runtime.uiSurfaces.size))
    yield ()
  }
end TuiDumpSpec
