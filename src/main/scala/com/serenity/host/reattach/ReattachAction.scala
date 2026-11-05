package com.serenity.host.reattach

import io.circe.Json

/** What the host does next with a message it has just been handed. */
enum ReattachAction:
  case ToServer(message: Json)
  case ToClient(message: Json)

  /** The attaching client asked for something the running server was not started for, so the server is replaced and
    * every message after this one in the same list goes to the new process.
    */
  case RestartServer
