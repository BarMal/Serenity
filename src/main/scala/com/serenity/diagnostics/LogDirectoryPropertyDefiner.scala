package com.serenity.diagnostics

import ch.qos.logback.core.PropertyDefinerBase

/** Gives logback.xml the platform's log directory as `${SERENITY_LOG_DIR}`, computed at configuration time so no launch
  * code has to run before the first log call.
  */
final class LogDirectoryPropertyDefiner extends PropertyDefinerBase:

  override def getPropertyValue: String = LogLocation.current.toString
