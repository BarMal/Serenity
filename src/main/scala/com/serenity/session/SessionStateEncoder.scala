package com.serenity.session

import java.util.concurrent.atomic.AtomicReference

import com.serenity.config.AppConfig
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}

/** Writes session state as compact JSON, encoding the config only when it is not the one written last.
  *
  * The config is mostly keymaps, hundreds of bindings that change on the rare edit, yet it was encoded afresh on every
  * save and made up most of what a save allocated. Owned by one manager rather than shared, so its single remembered
  * config belongs to the one session it saves.
  */
final private[session] class SessionStateEncoder:

  private val lastConfig = new AtomicReference[Option[(AppConfig, Json)]](None)

  private val encoder: Encoder[SessionState] = sessionStateEncoder(Encoder.instance(configJson))

  def compact(state: SessionState): String =
    state.asJson(using encoder).noSpaces

  private def configJson(config: AppConfig): Json =
    lastConfig.get match
      case Some((seen, encoded)) if seen == config => encoded
      case _ =>
        val encoded = SessionConfigCodec.encode(config)
        lastConfig.set(Some(config -> encoded))
        encoded
