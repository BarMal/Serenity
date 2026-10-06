package com.serenity.lsp

import com.serenity.lsp.client.{DocumentUri, LspProtocol}
import com.serenity.lsp.model.TextDocumentSyncKind
import com.serenity.rope.{Balance, Rope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `didChange` built from ropes is the notification built from their text, for every sync kind a server can negotiate:
  * the text is only collected when the kind needs it, and incremental sync never collects it at all.
  */
class LspProtocolRopeDidChangeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val uri = DocumentUri("file:///workspace/Foo.scala")

  private val edits = List(
    "object Foo"              -> "object Foo2",
    "object Foo\nval x = 1\n" -> "object Foo\nval x = 12\n",
    "line one\nline two"      -> "line one\nline TWO",
    ""                        -> "new document",
    "gone"                    -> "",
    "same"                    -> "same"
  )

  List(TextDocumentSyncKind.Full, TextDocumentSyncKind.Incremental, TextDocumentSyncKind.None).foreach { kind =>
    s"didChangeParams from ropes under $kind sync" should "equal the params built from their text" in
      edits.foreach { (before, after) =>
        LspProtocol.didChangeParams(uri, 7, Rope(before), Rope(after), kind) shouldBe
          LspProtocol.didChangeParams(uri, 7, before, after, kind)
      }
  }
