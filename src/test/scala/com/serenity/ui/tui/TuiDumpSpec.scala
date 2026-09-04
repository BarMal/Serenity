package com.serenity.ui.tui

import cats.effect.IO
import cats.syntax.all.*

class TuiDumpSpec extends TuiSpec:

  private def dump(label: String): TuiScript[Unit] =
    screen.flatMapF(current =>
      IO {
        println(s"===== $label =====")
        current.paintedRows.foreach((row, line) => println(f"$row%3d|$line"))
        println(s"caret=${current.caret} visible=${current.caretVisible}")
      }
    )

  "dump" should "show emitted bytes and gutter widths" in
    runTui(TuiEnvironment.withLines(12)) {
      for
        first <- screen
        _ <- liftIO(IO {
          println("FIRST-EMITTED=" + first.emitted.take(220).replace(0x1b.toChar.toString, "<E>"))
          println("ROW1=[" + first.rowText(1).stripTrailing + "]")
          println("ROW10=[" + first.rowText(10).stripTrailing + "]")
          println("ROW12=[" + first.rowText(12).stripTrailing + "]")
          println("CARET=" + first.caret)
        })
      yield ()
    }

  it should "show a 120-line gutter" in
    runTui(TuiEnvironment.withLines(120)) {
      verify("gutter")(s => println("ROW1-120=[" + s.rowText(1).stripTrailing + "] ROW11=[" + s.rowText(11).stripTrailing + "]"))
    }

  it should "show a long wrapped line" in
    runTui(TuiEnvironment.withFile("x" * 500)) {
      verify("wrap") { s =>
        (0 until 8).foreach(r => println(f"WRAP$r%d=[" + s.rowText(r).take(20) + "]"))
        println("CODEPOINT-1-1=" + s.cellAt(1, 2).codePoint)
      }
    }

  it should "show an opened file" in
    runTui(TuiEnvironment.withFile("alpha\nbeta\ngamma")) {
      dump("opened file")
    }

  it should "show the start page" in runTuiStartPage(dump("start page"))

  it should "show the command palette" in runTui() {
    for
      _ <- ctrl('p')
      _ <- dump("palette open")
      _ <- typeText("line")
      _ <- dump("palette filtered")
      _ <- arrowDown
      _ <- dump("palette after down arrow")
    yield ()
  }

  it should "show the settings surface" in runTui() {
    for
      _ <- ctrl('p')
      _ <- typeText("open settings")
      _ <- enter
      _ <- dump("settings")
    yield ()
  }

  it should "show two tabs" in runTui(TuiEnvironment.withFile("first file")) {
    for
      _ <- ctrl('t')
      _ <- typeText("second")
      _ <- dump("two tabs")
    yield ()
  }
end TuiDumpSpec
