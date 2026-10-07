package com.serenity.testkit

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.util.Try

import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.spellcheck.{DictionaryCache, DictionaryLoader}

/** One warm dictionary cache for every test `StateManager` in the JVM that checks against `spellCheck`.
  *
  * Spell check is on by default, so each manager parses the bundled dictionary on a background thread after its first
  * commit, and a spec that never shuts its manager down leaves that parse running. A suite builds hundreds of managers,
  * so on a small runner the parses starve the specs that wait on a lane or a quit (FileIoLanesSpec,
  * CloseWorkflowStateManagerSpec). Production keeps one cache per manager (#1677); tests that are not about dictionary
  * loading share this one.
  *
  * Specs ask for the cache from inside an IO as often as from a test thread, so the parse runs on a thread of its own
  * and callers wait for it through `Await`, which tells the IO runtime the thread is blocked: a compute thread waiting
  * here is replaced rather than lost, and a pool full of them cannot stall every other fiber for the parse.
  */
final class SharedDictionary(spellCheck: SpellCheckConfig, warmUp: (SpellCheckConfig, DictionaryCache) => Unit):

  private val cache = DictionaryCache()

  private val warmed: Future[Unit] =
    val done = Promise[Unit]()
    val warm: Runnable = () =>
      val _ = done.complete(Try(warmUp(spellCheck, cache)))
    val parser = Thread(warm, "shared-dictionary-warm-up")
    parser.setDaemon(true)
    parser.start()
    done.future

  /** The shared warm cache when `config` checks against this dictionary, a cache of its own otherwise. */
  def cacheFor(config: AppConfig): DictionaryCache =
    if config.languageToolsConfig.spellCheck == spellCheck then
      Await.result(warmed, Duration.Inf)
      cache
    else DictionaryCache()

object SharedDictionary:

  private val shared = SharedDictionary(AppConfig.default.languageToolsConfig.spellCheck, parse)

  private def parse(spellCheck: SpellCheckConfig, cache: DictionaryCache): Unit =
    DictionaryLoader.loadSnapshot(spellCheck, cache).context.stems.foreach(_.prepareSuggestions())

  def cacheFor(config: AppConfig): DictionaryCache = shared.cacheFor(config)

  def default: DictionaryCache = cacheFor(AppConfig.default)
