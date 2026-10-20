package com.serenity.lsp

import com.serenity.lsp.client.DocumentUri
import com.serenity.rope.Rope

/** What `LspManager` remembers about a document the editor has open.
  *
  * Only a document some server has been told about needs its text kept: the next `didChange` is computed against it,
  * and a restarted server is reopened from it. A document without a server has no one to send either to, so it is
  * remembered by its URI alone, however large it is and however often it is edited.
  */
private[lsp] enum OpenDocument:
  case Served(text: Rope, version: Option[Long] = None)
  case Unserved

private[lsp] object OpenDocument:

  type Registry = Map[DocumentUri, OpenDocument]

  extension (registry: Registry)

    def textOf(uri: DocumentUri): Option[Rope] =
      registry.get(uri).collect { case Served(text, _) => text }

    /** The editor's content version of the text last sent, when it was sent as an edit of a known version. */
    def versionOf(uri: DocumentUri): Option[Long] =
      registry.get(uri).collect { case Served(_, version) => version }.flatten

    def served(uri: DocumentUri, text: Rope, version: Option[Long] = None): Registry =
      registry.updated(uri, Served(text, version))

    def unserved(uri: DocumentUri): Registry = registry.updated(uri, Unserved)

    /** The registry after `uri` changed to `text` while no connection was serving it: `text` is kept only when a server
      * is still attached to the document but down (its restart reopens the document from the latest text). Also reports
      * whether the document was already known, so its absence of a server is announced once.
      */
    def changedWithoutConnection(uri: DocumentUri, text: Rope, awaitsServer: Boolean): (Registry, Boolean) =
      (if awaitsServer then served(uri, text) else unserved(uri), registry.contains(uri))
