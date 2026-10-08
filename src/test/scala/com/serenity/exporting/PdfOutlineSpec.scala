package com.serenity.exporting

import com.serenity.manuscript.layout.PaginatorFixture.*
import com.serenity.manuscript.layout.{PageKind, PagedDocument}
import com.serenity.manuscript.{Section, SectionHeading}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PdfOutlineSpec extends AnyFlatSpec with Matchers:

  private def sectionPages(paged: PagedDocument): Vector[Int] =
    paged.pages.indices.filter(paged.pages(_).kind == PageKind.SectionStart).toVector

  "The PDF outline" should "hold one entry per chapter, titled by its heading and aimed at the page it starts" in {
    val m     = manuscript(Vector(chapter("Arrival", paragraphOf('a', 40)), chapter("Departure", paragraphOf('b', 3))))
    val paged = paginate(m)

    val entries = PdfOutline.entries(m, paged)

    entries.map(_.title) shouldBe Vector("Arrival", "Departure")
    entries.map(_.pageIndex) shouldBe sectionPages(paged)
    entries.map(_.children) shouldBe Vector(Vector.empty, Vector.empty)
  }

  it should "join the lines of a multi-line heading with a space" in {
    val heading = Section.Chapter(Some(SectionHeading(Vector("Chapter 1", "The Arrival"))), Vector(prose("Text.")))
    val m       = manuscript(Vector(heading))

    PdfOutline.entries(m, paginate(m)).map(_.title) shouldBe Vector("Chapter 1 The Arrival")
  }

  it should "nest the chapters of a titled part beneath it" in {
    val part = Section.Part(
      Some(SectionHeading.of("Part One")),
      Vector(chapter("Arrival", prose("a")), chapter("Departure", prose("b")))
    )
    val m     = manuscript(Vector(part, chapter("Epilogue", prose("c"))))
    val paged = paginate(m)

    val entries = PdfOutline.entries(m, paged)

    entries.map(_.title) shouldBe Vector("Part One", "Epilogue")
    entries.head.children.map(_.title) shouldBe Vector("Arrival", "Departure")
    entries.head.pageIndex shouldBe sectionPages(paged).head
    entries.head.children.map(_.pageIndex) shouldBe sectionPages(paged).slice(1, 3)
  }

  it should "lift the chapters of an untitled part to the level of the part" in {
    val part = Section.Part(None, Vector(chapter("Arrival", prose("a")), chapter("Departure", prose("b"))))
    val m    = manuscript(Vector(part))

    PdfOutline.entries(m, paginate(m)).map(_.title) shouldBe Vector("Arrival", "Departure")
  }

  it should "give an untitled chapter no entry and keep the later entries on their own pages" in {
    val untitled = Section.Chapter(None, Vector(prose("No heading here.")))
    val m        = manuscript(Vector(chapter("First", prose("a")), untitled, chapter("Third", prose("c"))))
    val paged    = paginate(m)

    val entries = PdfOutline.entries(m, paged)

    entries.map(_.title) shouldBe Vector("First", "Third")
    entries.map(_.pageIndex) shouldBe Vector(0, 2)
    paged.pages.map(_.kind) shouldBe Vector.fill(3)(PageKind.SectionStart)
  }

  it should "bookmark a heading that cleaning changes, on the page it opens" in {
    val m       = manuscript(Vector(chapter("First", prose("a")), chapter("Chap\u0000ter\tTwo", prose("b"))))
    val entries = PdfOutline.entries(m, paginate(m))

    entries.map(_.title) shouldBe Vector("First", "Chapter Two")
    entries.map(_.pageIndex) shouldBe Vector(0, 1)
  }

  it should "keep two chapters of the same heading on their own pages" in {
    val m = manuscript(Vector(chapter("Same", prose("a")), chapter("Same", prose("b"))))

    PdfOutline.entries(m, paginate(m)).map(_.pageIndex) shouldBe Vector(0, 1)
  }

  it should "not confuse a heading with an untitled chapter whose text begins the same way" in {
    val untitled = Section.Chapter(None, Vector(prose("A")))
    val m        = manuscript(Vector(untitled, chapter("A Tale", prose("b"))))

    PdfOutline.entries(m, paginate(m)).map(e => e.title -> e.pageIndex) shouldBe Vector("A Tale" -> 1)
  }

  it should "be empty for a manuscript with no headings" in {
    val m = manuscript(Vector(Section.Chapter(None, Vector(prose("Just text.")))))

    PdfOutline.entries(m, paginate(m)) shouldBe Vector.empty
  }
