package com.serenity.perf

import com.serenity.config.AppConfig
import com.serenity.spellcheck.DictionaryCache
import com.serenity.testkit.SharedDictionary

/** The dictionary a benchmark session's spell check checks against, loaded before any timing starts.
  *
  * Spell check is on by default, so every benchmark `StateManager` analyses its prose against it. A session pays for
  * loading it once, at startup, not on every frame or in every one of the dozen sessions a benchmark run builds: one
  * cache is shared between them and filled before timing, and what the benchmarks then measure is the steady state,
  * spell checking included.
  */
private[perf] object WarmDictionary:

  def cacheFor(config: AppConfig): DictionaryCache = SharedDictionary.cacheFor(config)
