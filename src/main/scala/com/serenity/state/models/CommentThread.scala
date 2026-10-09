package com.serenity.state.models

import java.time.Instant

import cats.Order

/** Identifies a comment within its buffer for as long as the comment lives: edits move its range and deletions shift
  * its position in the list, but neither changes its id, so a lens, a command or a saved session can name it without
  * depending on where it sits.
  */
opaque type CommentId = Int

object CommentId:

  /** What a comment carries before `Annotations` has given it an id; never a live comment's id. */
  val Unassigned: CommentId = 0

  def apply(value: Int): CommentId = value

  extension (id: CommentId)
    def value: Int                       = id
    def isAssigned: Boolean              = id != Unassigned
    def next: CommentId                  = id + 1
    def max(other: CommentId): CommentId = math.max(id, other)

  given Order[CommentId] = Order.by(identity)

/** One answer in a comment's thread. */
final case class CommentReply(author: String, at: Instant, text: String)
