package com.serenity.lsp.client

/** The `id` of a JSON-RPC request/response, distinct from any other `Long` a connection handles (document versions,
  * line/character offsets) so a mixed-up call site fails to compile instead of silently keying the wrong map.
  */
opaque type RequestId = Long

object RequestId:
  def apply(value: Long): RequestId = value

  extension (id: RequestId) def value: Long = id

/** The `uri` of an open document, distinct from a [[WorkspaceRootUri]] -- both are `file://...` strings, but mixing
  * them up (e.g. keying a per-document map by workspace root) is exactly the class of bug an opaque type prevents.
  */
opaque type DocumentUri = String

object DocumentUri:
  def apply(value: String): DocumentUri = value

  extension (uri: DocumentUri) def value: String = uri

/** The `rootUri` of a workspace passed to `initialize`, distinct from a [[DocumentUri]] (see there for why). */
opaque type WorkspaceRootUri = String

object WorkspaceRootUri:
  def apply(value: String): WorkspaceRootUri = value

  extension (uri: WorkspaceRootUri) def value: String = uri

/** A JSON-RPC `method` name, e.g. `"textDocument/hover"`. */
opaque type LspMethod = String

object LspMethod:
  def apply(value: String): LspMethod = value

  extension (method: LspMethod) def value: String = method
