# Remote Storage Decision

## Decision

SFTP is Serenity's first remote-storage backend.

SFTP is a deliberately narrow first step: it provides filesystem-like read, write, stat, list, and directory-creation
operations over an established protocol without committing Serenity to a provider API, a cloud account model, or a
generic plugin registry. WebDAV, object storage, and provider-specific APIs remain separate future decisions.

The first implementation is an online, text-document workflow. It does not provide a remote filesystem mount, offline
editing queue, background synchronization, or a general-purpose remote provider marketplace.

## Location and identity

The canonical document URI is:

```text
sftp://[user@]host[:port]/absolute/path/to/document.md
```

Rules:

- The scheme is case-insensitive and normalized to `sftp`.
- The default port is 22 when it is omitted.
- The user is optional in the URI; when omitted, the local SSH identity supplies the username.
- The path is an absolute path on the SFTP server. A URI without a path identifies the server root and is not an
  openable document.
- URI query and fragment components are rejected. They must not become an authentication or configuration channel.
- A remote identity is the normalized tuple `(scheme, host, port, username, absolute path)`.
- Hostnames are compared case-insensitively; paths are not case-folded.

The existing `StorageLocation.Remote` model recognizes remote URIs, while editor buffers, recent files, and session
serialization currently use `Path`. The implementation work must preserve the complete SFTP URI as the durable
identity rather than converting it through `Path`.

## Authentication and trust

The first backend accepts authentication from the operating-system SSH agent only. Serenity must never collect,
write, or serialize an SSH password, private key, passphrase, access token, or session credential.

- Authentication succeeds only when the SSH agent can authenticate the requested user.
- The implementation uses the user's existing SSH configuration for agent selection and connection options, but the
  SFTP URI remains the source of document identity.
- Server host keys are verified against the user's known-hosts database. Unknown keys and changed keys fail with a
  visible authentication/trust error; Serenity must not silently accept or persist them.
- Host-key prompts, if a future UX supports them, must be an explicit user action and must update the user's SSH
  trust store through the platform SSH mechanism rather than Serenity config or session files.
- Authentication failures are typed as `AuthenticationFailed`; authorization failures are surfaced as access denied.

This keeps secrets outside Serenity's existing plaintext config and JSON session formats. A future OS-keychain or
explicit private-key UX requires a separate security decision.

## Required operations and semantics

The SFTP provider must implement the existing provider boundary and add only the shared operations that both local and
SFTP workflows require:

| Operation | Contract |
| --- | --- |
| `stat` | Add the shared metadata operation needed by both providers. Return display name, byte size, modification time, file/directory kind, and a provider revision. Missing paths return not found. |
| `list` | Stream direct children only. Each entry carries a complete SFTP URI and metadata; listing a file is an error. |
| `create-directory` | Add the shared directory-creation operation needed by both providers. Create one requested directory and any missing parents needed by Save As. Existing directories are success; a file at any requested segment is an error. |
| `open` | Open UTF-8 text documents only, stream the transfer, and return metadata plus the revision observed at open. Binary content and invalid UTF-8 are rejected before creating a buffer. |
| `save` | Require the expected revision for an existing remote document. Write to a same-directory temporary file, then replace the target with the server's rename operation. Never truncate the target before the complete upload succeeds. |
| `copy` | Copy only within SFTP locations owned by the same provider/session. Preserve text bytes and report the destination metadata. |

The provider maps failures to the existing neutral error model: missing path, access denied, authentication failure,
offline/transport failure, cancellation, conflict, or a diagnostic failure without exposing SDK exceptions to editor
state.

## Revision and conflicts

An opened document receives a revision containing the remote file's size, modification timestamp, and SHA-256 content
digest. Before Save, the provider rechecks the remote metadata and digest. If the observed revision differs from the
expected revision, Save returns `Conflict` without replacing the remote file.

The temporary upload name is unique, is created beside the target, and is removed best-effort after success or failure.
The final rename must not be reported as successful unless the target replacement completed. If the server cannot
provide the required replacement semantics, the operation fails rather than silently degrading to truncate-and-write.

## Timeouts, cancellation, and retries

- Connection and authentication timeout: 10 seconds.
- Each stat, list, directory-creation, open, upload, download, rename, or copy operation timeout: 30 seconds.
- Cats Effect cancellation closes the active SFTP channel and removes any temporary upload when possible.
- Automatic retries are limited to one retry for idempotent stat/list/open reads after a transport disconnect, with a
  bounded backoff. Save, rename, and copy are never automatically retried because completion may be ambiguous.
- A timeout or disconnect leaves the buffer dirty and reports a recoverable status. It does not overwrite the local
  editor content or claim that a save completed.

## Offline behavior and size limits

The first backend has no offline cache and no queued writes. A remote document that is already open remains editable in
memory while disconnected, but every remote read or save fails with a visible offline/transport status. Restarting
Serenity does not promise restoration of a remote document unless its session contains the persisted unsaved text and
the URI identity.

The first implementation supports UTF-8 text documents up to 50 MiB. Transfers use bounded streaming, and the
provider rejects a larger remote document before materializing it in an editor buffer. Serenity may retain the loaded
text in its existing in-memory editor model after the bounded transfer completes; streaming does not imply incremental
editing or remote partial writes.

## Implementation issue breakdown

These are the smallest follow-up slices needed to implement the decision:

1. **Add an SFTP transport adapter.**
   - Add one maintained SFTP client dependency and an effect-safe connection resource.
   - Implement SSH-agent authentication, known-host verification, the timeout policy, cancellation, and typed error
     mapping.
   - Test successful authentication, rejected host keys, authentication/access failures, cancellation, timeout, and
     disconnect behavior without storing credentials.

2. **Extend the document-storage boundary and implement `SftpDocumentStorageProvider`.**
   - Add only the shared `stat` and `create-directory` operations required by local and SFTP workflows, then parse and
     normalize the canonical URI and implement stat/list/open/save/copy/create-directory semantics.
   - Enforce UTF-8 and the 50 MiB limit, optimistic conflict detection, same-directory temporary uploads, and safe
     replacement.
   - Use an in-process fake SFTP server or protocol test fixture; do not make tests depend on a developer's SSH host.

3. **Replace Path-only document identity at the file boundary.**
   - Let open and Save As route local paths to the local provider and SFTP URIs to the SFTP provider.
   - Preserve existing local format codecs and native local file dialogs.
   - Cover open, save, Save As, directory creation, missing paths, conflicts, cancellation, and failure status at the
     workflow boundary.

4. **Persist and browse remote identities.**
   - Store SFTP URIs, not `Path` conversions, in buffers, recent files, and sessions.
   - Restore remote buffers with their URI and persisted unsaved text; do not perform network IO during session decode.
   - Make file-browser navigation and recent-file display distinguish remote locations without leaking credentials.

5. **Add user-facing remote workflow states.**
   - Show authentication, trust, offline, conflict, size-limit, and unsupported-format outcomes with actionable text.
   - Keep dirty buffers and local edits intact on every failed or ambiguous write.
   - Document the SSH-agent and known-host prerequisites in the user-facing help surface.

Each slice must retain the existing local behavior and add focused tests before implementation. No slice should add a
generic provider registry, a second remote backend, plaintext secret persistence, or an offline synchronization queue.

## Existing implementation anchors

- `StorageLocation` separates local paths from recognized remote URIs but currently marks all remote locations
  unsupported.
- `DocumentStorageProvider` already supplies provider-neutral metadata, revisions, list/open/save/copy operations, and
  typed storage failures; only `LocalDocumentStorageProvider` is implemented.
- `Buffer.filePath`, `AppState.recentFiles`, and `SessionBuffer.filePath` are currently `Path`/string projections that
  require URI-preserving changes in the implementation slices above.
- File workflows currently reject remote open and Save As targets with a visible status rather than attempting a
  network operation.

See `src/main/scala/com/serenity/io/StorageLocation.scala`, `src/main/scala/com/serenity/io/DocumentStorage.scala`,
`src/main/scala/com/serenity/state/models/Buffer.scala`, `src/main/scala/com/serenity/state/models/AppState.scala`,
`src/main/scala/com/serenity/session/SessionState.scala`, and
`src/main/scala/com/serenity/state/manager/StateManagerWorkflowCapability.scala` for the current boundaries.
