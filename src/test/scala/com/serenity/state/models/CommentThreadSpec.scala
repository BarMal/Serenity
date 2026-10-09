package com.serenity.state.models

import java.time.Instant

import com.serenity.document.RenderedComment
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommentThreadSpec extends AnyFlatSpec with Matchers:

  private val at = Instant.parse("2026-10-06T10:00:00Z")

  private def comment(text: String, id: CommentId = CommentId.Unassigned): DocumentComment =
    DocumentComment(CursorPosition(0, 0), CursorPosition(0, 3), text, id = id)

  "Annotations.withNewComment" should "give each new comment the next id" in {
    val annotations = Annotations().withNewComment(comment("a")).withNewComment(comment("b"))

    annotations.documentComments.map(_.id) shouldBe List(CommentId(1), CommentId(2))
  }

  it should "never give a deleted comment's id to a later one" in {
    val annotations = Annotations().withNewComment(comment("a")).withNewComment(comment("b"))

    val after = annotations.withoutComment(CommentId(2)).withNewComment(comment("c"))

    after.documentComments.map(comment => comment.id -> comment.text) shouldBe
      List(CommentId(1) -> "a", CommentId(3) -> "c")
  }

  it should "skip ids the list already uses even when the counter was never advanced" in {
    val annotations = Annotations(documentComments = List(comment("kept", CommentId(7))))

    annotations.withNewComment(comment("new")).documentComments.map(_.id) shouldBe List(CommentId(7), CommentId(8))
  }

  "Annotations.withCommentIdsAssigned" should "give comments without ids fresh, distinct ones" in {
    val annotations = Annotations(documentComments = List(comment("a"), comment("b"), comment("c")))

    annotations.withCommentIdsAssigned.documentComments.map(_.id) shouldBe
      List(CommentId(1), CommentId(2), CommentId(3))
  }

  it should "keep the ids comments already have and number the rest around them" in {
    val annotations = Annotations(documentComments = List(comment("a"), comment("b", CommentId(5)), comment("c")))

    annotations.withCommentIdsAssigned.documentComments.map(comment => comment.text -> comment.id) shouldBe
      List("a" -> CommentId(6), "b" -> CommentId(5), "c" -> CommentId(7))
  }

  it should "re-number a comment that repeats an earlier comment's id" in {
    val annotations = Annotations(documentComments = List(comment("a", CommentId(2)), comment("b", CommentId(2))))

    val assigned = annotations.withCommentIdsAssigned.documentComments

    assigned.map(_.id).distinct should have size 2
    assigned.map(_.text) shouldBe List("a", "b")
    assigned.head.id shouldBe CommentId(2)
  }

  it should "leave the next id past every id in use" in {
    val assigned = Annotations(documentComments = List(comment("a"), comment("b", CommentId(4)))).withCommentIdsAssigned

    assigned.nextCommentId.value should be > assigned.documentComments.map(_.id.value).max
  }

  "Annotations.comment" should "find a comment by id wherever it sits in the list" in {
    val annotations = Annotations(documentComments = List(comment("a", CommentId(1)), comment("b", CommentId(2))))

    annotations.comment(CommentId(2)).map(_.text) shouldBe Some("b")
    annotations.comment(CommentId(9)) shouldBe None
  }

  "Annotations.withUpdatedComment" should "change only the comment with that id" in {
    val annotations = Annotations(documentComments = List(comment("a", CommentId(1)), comment("a", CommentId(2))))

    val updated = annotations.withUpdatedComment(CommentId(2))(_.copy(text = "changed"))

    updated.documentComments.map(_.text) shouldBe List("a", "changed")
  }

  "Annotations.shownComments" should "leave out resolved comments unless asked for them" in {
    val open     = comment("open", CommentId(1))
    val resolved = comment("done", CommentId(2)).resolve
    val all      = Annotations(documentComments = List(open, resolved))

    all.shownComments(showResolved = false) shouldBe List(open)
    all.shownComments(showResolved = true) shouldBe List(open, resolved)
  }

  "A DocumentComment thread" should "gather replies in the order they were written" in {
    val first  = CommentReply("Ada", at, "One")
    val second = CommentReply("Grace", at.plusSeconds(60), "Two")

    comment("root").withReply(first).withReply(second).replies shouldBe List(first, second)
  }

  it should "resolve and reopen without losing its replies" in {
    val threaded = comment("root").withReply(CommentReply("Ada", at, "One"))

    threaded.resolve.reopen shouldBe threaded
    threaded.resolve.resolved shouldBe true
  }

  it should "record an edit time only when the text changed" in {
    val original = comment("root")

    original.withText("root", at) shouldBe original
    original.withText("new", at).editedAt shouldBe Some(at)
  }

  "A comment lens" should "head itself with the comment's author and resolved state" in {
    val target = CommentLensTarget(CommentId(1), comment("root", CommentId(1)).copy(author = Some("Ada")).resolve)

    lens(Some(target)).headline shouldBe "comment · Ada · resolved"
  }

  it should "keep the plain title for a comment with no recorded author" in {
    lens(Some(CommentLensTarget(CommentId(1), comment("root", CommentId(1))))).headline shouldBe "comment"
    lens(None).headline shouldBe "comment"
  }

  it should "list each reply under the draft, indenting a reply's later lines" in {
    val replies = List(CommentReply("Grace", at, "Agreed"), CommentReply("Ada", at, "First line\nsecond line"))
    val target  = CommentLensTarget(CommentId(1), comment("root", CommentId(1)).copy(replies = replies))

    lens(Some(target)).threadLines shouldBe List("Grace: Agreed", "Ada: First line", "  second line")
  }

  private def lens(target: Option[CommentLensTarget]): CommentLensState =
    CommentLensState(RenderedComment(0, "root", "root"), "root", 4, target)
