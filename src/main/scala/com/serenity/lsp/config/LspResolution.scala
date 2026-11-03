package com.serenity.lsp.config

/** Why a language does or does not have a language server to run (#2523). Each case asks something different of the
  * user, which a bare `None` hides: `NoServerExists` and `Disabled` are no one's fault and need no notice, while
  * `NotInstalled` is something the user can fix.
  */
enum LspResolution:

  /** The registry has no server for this language at all. */
  case NoServerExists

  /** The user turned this language's server off. */
  case Disabled

  /** The server is configured, but its command is not on the path. */
  case NotInstalled(server: LspServerConfig)

  case Available(server: LspServerConfig)

  def availableServer: Option[LspServerConfig] =
    this match
      case Available(server)                           => Some(server)
      case NoServerExists | Disabled | NotInstalled(_) => None
