package com.serenity.host

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.HexFormat

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/** What a hosted server was started as. A running host is reused only when a launch asks for the same thing. */
final case class LaunchIdentity(argv: List[String], cwd: String, envFingerprint: String, initializeFingerprint: String)

object LaunchIdentity:
  given Codec[LaunchIdentity] = deriveCodec

enum Difference:
  case Argv, WorkingDirectory, Environment, Initialize, ProtocolVersion

enum Compatibility:
  case Reusable
  case Mismatch(differences: List[Difference])

object Compatibility:

  def check(
    running: LaunchIdentity,
    runningProtocol: Int,
    requested: LaunchIdentity,
    requestedProtocol: Int
  ): Compatibility =
    val differences = List(
      Difference.Argv             -> (running.argv != requested.argv),
      Difference.WorkingDirectory -> (running.cwd != requested.cwd),
      Difference.Environment      -> (running.envFingerprint != requested.envFingerprint),
      Difference.Initialize       -> (running.initializeFingerprint != requested.initializeFingerprint),
      Difference.ProtocolVersion  -> (runningProtocol != requestedProtocol)
    ).collect { case (difference, true) => difference }
    if differences.isEmpty then Reusable else Mismatch(differences)

/** A digest of the environment variables that change what a server does. A host started under one `JAVA_HOME` must not
  * silently serve a launch under another.
  */
object EnvFingerprint:

  def of(env: Map[String, String], keys: Set[String]): String =
    val entries =
      keys.toList.sorted.flatMap(key => env.get(key).map(value => lengthPrefixed(key) + lengthPrefixed(value)))
    val digest = MessageDigest.getInstance("SHA-256").digest(entries.mkString.getBytes(StandardCharsets.UTF_8))
    HexFormat.of().formatHex(digest)

  private def lengthPrefixed(text: String): String = s"${text.length}:$text"
