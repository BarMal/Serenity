package com.serenity.lsp

import com.serenity.rope.{ChangeSet, Rope}

/** What an editor commit did to a document: `change` took its text at content version `fromVersion` to `toVersion`. The
  * versions are what let a receiver tell that the change starts from the text it holds, rather than hoping so from the
  * lengths.
  */
final case class DocumentDelta(change: ChangeSet, fromVersion: Long, toVersion: Long):

  /** The one delta that does both, or `None` when `next` does not start where this one ends. */
  def andThen(next: DocumentDelta): Option[DocumentDelta] =
    Option
      .when(toVersion == next.fromVersion)(change.compose(next.change))
      .flatten
      .map(DocumentDelta(_, fromVersion, next.toVersion))

  /** `held`, a document at `heldVersion`, is what this delta applies to, and `after` is what it produces. */
  def appliesTo(heldVersion: Option[Long], held: Rope, after: Rope): Boolean =
    heldVersion.contains(fromVersion) && change.oldLength == held.weight && change.newLength == after.weight
