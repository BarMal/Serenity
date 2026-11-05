package com.serenity.host.reattach

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.HexFormat

import io.circe.Json

/** A digest of what an `initialize` request asks for. A running server may answer a later client from its cached result
  * only if the request is the same in every way the server could notice.
  *
  * `processId` and `workDoneToken` change on every launch or request without changing what the server is asked to do,
  * so they are left out. Object keys are sorted so the digest does not depend on how a client orders them.
  */
object InitializeFingerprint:

  private val PerLaunchFields = Set("processId", "workDoneToken")

  def of(params: Json): String =
    val stable    = params.mapObject(_.filterKeys(!PerLaunchFields.contains(_)))
    val canonical = sortKeys(stable).noSpaces.getBytes(StandardCharsets.UTF_8)
    HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(canonical))

  private def sortKeys(json: Json): Json =
    json.fold(
      Json.Null,
      Json.fromBoolean,
      Json.fromJsonNumber,
      Json.fromString,
      elements => Json.fromValues(elements.map(sortKeys)),
      fields => Json.fromFields(fields.toList.sortBy(_._1).map((name, value) => name -> sortKeys(value)))
    )
