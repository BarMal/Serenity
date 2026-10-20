package com.serenity.lsp

import java.io.{ByteArrayInputStream, OutputStream}
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.lsp.client.LspStderrLog
import com.serenity.lsp.client.LspStderrLog.{FileStorage, Storage}
import com.serenity.lsp.config.LanguageId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** File-system operations are the cost that grows with the size of the server's stderr, on every platform and most on
  * those with slow syncs, so their number per megabyte is what is pinned here.
  */
class LspStderrLogSpec extends AnyFlatSpec with Matchers:

  private val megabyte = 1024 * 1024
  private val maxBytes = 64L * 1024

  final private class CountingStorage extends Storage:
    val writes   = new AtomicInteger(0)
    val opens    = new AtomicInteger(0)
    val replaces = new AtomicInteger(0)

    def size(path: Path): Long = FileStorage.size(path)

    def open(path: Path): OutputStream =
      opens.incrementAndGet()
      val underlying = FileStorage.open(path)
      new OutputStream:
        def write(b: Int): Unit = write(Array(b.toByte), 0, 1)
        override def write(b: Array[Byte], o: Int, l: Int): Unit =
          writes.incrementAndGet()
          underlying.write(b, o, l)
        override def flush(): Unit = underlying.flush()
        override def close(): Unit = underlying.close()

    def replace(from: Path, to: Path): Unit =
      replaces.incrementAndGet()
      FileStorage.replace(from, to)

    def delete(path: Path): Unit = FileStorage.delete(path)

  private def drainMegabyte(storage: Storage): Path =
    val directory = TestTemp.directory("lsp-stderr-log-spec")
    val line      = ("x" * 99 + "\n").getBytes
    val stderr    = new ByteArrayInputStream(Array.fill(megabyte / line.length)(line).flatten)
    LspStderrLog
      .drain(stderr, LanguageId.Scala, directory, maxBytes, NoOpLogger[IO], storage)
      .unsafeRunSync()
    directory

  "LspStderrLog.drain" should "write a megabyte in a bounded number of writes, however small the server's lines" in {
    val storage   = new CountingStorage
    val directory = drainMegabyte(storage)

    storage.writes.get should be <= 2 * (megabyte / LspStderrLog.BatchBytes) + 2
    storage.opens.get shouldBe storage.replaces.get + 1
    storage.replaces.get should be <= 2 * (megabyte / maxBytes.toInt)
    Files.size(LspStderrLog.pathFor(directory, LanguageId.Scala)) should be <= maxBytes
  }
