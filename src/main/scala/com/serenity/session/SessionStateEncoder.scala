package com.serenity.session

import java.lang.StringBuilder as JStringBuilder
import java.util.UUID
import java.util.concurrent.atomic.AtomicReference

import com.serenity.config.AppConfig
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}

/** Writes session state as compact JSON, printing the config only when it is not the one written last.
  *
  * The config is mostly keymaps, hundreds of bindings that change on the rare edit, yet it was encoded and printed
  * afresh on every save and made up most of what a save allocated, even once its JSON tree was remembered: printing it
  * again cost several times what the rest of the state did. So the printed text is remembered and spliced in where the
  * config belongs. Owned by one manager rather than shared, so its single remembered config belongs to the one session
  * it saves.
  */
final private[session] class SessionStateEncoder:

  private val lastConfig = new AtomicReference[Option[(AppConfig, String)]](None)

  // The printer writes a string verbatim between its quotes, so a token no session text can contain marks the splice.
  private val configMarker = s"serenity-config-${UUID.randomUUID}"
  private val markedConfig = "\"" + configMarker + "\""

  private val encoderWithoutConfig: Encoder[SessionState] =
    sessionStateEncoder(Encoder.instance(_ => Json.fromString(configMarker)))

  def compact(state: SessionState): String =
    val printed = state.asJson(using encoderWithoutConfig).noSpaces
    val config  = configText(state.config)
    val at      = printed.indexOf(markedConfig)
    new JStringBuilder(printed.length + config.length)
      .append(printed, 0, at)
      .append(config)
      .append(printed, at + markedConfig.length, printed.length)
      .toString

  private def configText(config: AppConfig): String =
    lastConfig.get match
      case Some((seen, text)) if seen == config => text
      case _ =>
        val text = SessionConfigCodec.encode(config).noSpaces
        lastConfig.set(Some(config -> text))
        text
