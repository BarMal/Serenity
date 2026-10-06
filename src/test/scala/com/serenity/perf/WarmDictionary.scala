package com.serenity.perf

import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.spellcheck.{DictionaryCache, DictionaryLoader}

/** The dictionary a benchmark session's spell check checks against, loaded before any timing starts.
  *
  * Spell check is on by default, so every benchmark `StateManager` analyses its prose against it. A session pays for
  * loading it once, at startup, not on every frame or in every one of the dozen sessions a benchmark run builds: one
  * cache is shared between them and filled here, and what the benchmarks then measure is the steady state, spell
  * checking included.
  */
private[perf] object WarmDictionary:

  private lazy val shared: (SpellCheckConfig, DictionaryCache) =
    val spellCheck = AppConfig.default.languageToolsConfig.spellCheck
    val cache      = DictionaryCache()
    DictionaryLoader.loadSnapshot(spellCheck, cache).context.stems.foreach(_.prepareSuggestions())
    spellCheck -> cache

  /** The shared warm cache when `config` checks against the default dictionary, a cache of its own otherwise. */
  def cacheFor(config: AppConfig): DictionaryCache =
    val (spellCheck, cache) = shared
    if config.languageToolsConfig.spellCheck == spellCheck then cache else DictionaryCache()
