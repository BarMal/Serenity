package com.serenity.testkit

import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.spellcheck.{DictionaryCache, DictionaryLoader}

/** The default dictionary, parsed once for every test `StateManager` in the JVM that asks for it.
  *
  * Spell check is on by default, so each manager parses the bundled dictionary on a background thread after its first
  * commit, and a spec that never shuts its manager down leaves that parse running. A suite builds hundreds of managers,
  * so on a small runner the parses starve the specs that wait on a lane or a quit (FileIoLanesSpec,
  * CloseWorkflowStateManagerSpec). Production keeps one cache per manager (#1677); tests that are not about dictionary
  * loading share this one.
  */
object SharedDictionary:

  private lazy val shared: (SpellCheckConfig, DictionaryCache) =
    val spellCheck = AppConfig.default.languageToolsConfig.spellCheck
    val cache      = DictionaryCache()
    DictionaryLoader.loadSnapshot(spellCheck, cache).context.stems.foreach(_.prepareSuggestions())
    spellCheck -> cache

  /** The shared warm cache when `config` checks against the default dictionary, a cache of its own otherwise. */
  def cacheFor(config: AppConfig): DictionaryCache =
    val (spellCheck, cache) = shared
    if config.languageToolsConfig.spellCheck == spellCheck then cache else DictionaryCache()

  def default: DictionaryCache = cacheFor(AppConfig.default)
