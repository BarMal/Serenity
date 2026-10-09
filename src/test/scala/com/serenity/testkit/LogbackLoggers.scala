package com.serenity.testkit

import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory

/** The logback logger behind a name, for specs that attach an appender to capture what the code under test logs.
  *
  * A plain `LoggerFactory.getLogger` is not enough while suites run in parallel: SLF4J 2.0 hands a `SubstituteLogger`
  * to any thread that asks while another thread is still initialising it (`LoggerFactory.getProvider`), and a spec that
  * casts that to logback's `Logger` aborts. The initialising thread holds the `LoggerFactory` class monitor for the
  * whole of initialisation, so taking it first waits that out.
  */
object LogbackLoggers:

  def named(name: String): Logger =
    classOf[LoggerFactory].synchronized(LoggerFactory.getLogger(name)) match
      case logger: Logger => logger
      case other => throw new IllegalStateException(s"$name resolved to ${other.getClass.getName}, not logback")
