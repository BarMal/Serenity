package com.serenity.lsp

import com.serenity.lsp.OpenDocument.*
import com.serenity.lsp.client.DocumentUri
import com.serenity.rope.{Balance, Rope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `LspManager` keeps a document's text only while some server could still be sent it. */
class OpenDocumentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val uri         = DocumentUri("file:///workspace/Foo.scala")
  private val noDocuments = Map.empty[DocumentUri, OpenDocument]

  "A document with no server" should "be remembered by its uri alone, however often it changes" in {
    val registry = (1 to 100).foldLeft(noDocuments.unserved(uri)) { (current, version) =>
      current.changedWithoutConnection(uri, Rope(s"object Foo$version"), awaitsServer = false)._1
    }

    registry shouldBe Map(uri -> Unserved)
    registry.textOf(uri) shouldBe None
  }

  "A document whose server is down" should "keep its latest text for the restart to reopen it with" in {
    val registry = (1 to 3).foldLeft(noDocuments.served(uri, Rope("object Foo"))) { (current, version) =>
      current.changedWithoutConnection(uri, Rope(s"object Foo$version"), awaitsServer = true)._1
    }

    registry.textOf(uri).map(_.collect()) shouldBe Some("object Foo3")
  }

  "A change without a connection" should "report whether the document was already known" in {
    val (afterFirst, firstKnown) = noDocuments.changedWithoutConnection(uri, Rope("a"), awaitsServer = false)
    val (_, secondKnown)         = afterFirst.changedWithoutConnection(uri, Rope("ab"), awaitsServer = false)

    firstKnown shouldBe false
    secondKnown shouldBe true
  }

  "A served document" should "give up its text when its server turns out not to exist" in {
    val registry = noDocuments.served(uri, Rope("object Foo")).unserved(uri)

    registry shouldBe Map(uri -> Unserved)
  }
