package com.serenity.config

/** Static and dynamic keys understood by the text config format. */
object ConfigKeySchema:

  val dynamicPrefixes: List[String] =
    LanguageToolsConfig.Schema.dynamicPrefixes ++
      InputConfig.Schema.dynamicPrefixes

  def deprecatedReplacement(key: String): Option[String] =
    deprecatedKeys.get(key)

  def isKnownKey(key: String): Boolean =
    currentKeys.contains(key) ||
      deprecatedKeys.contains(key) ||
      dynamicPrefixes.exists(key.startsWith)

  val currentKeys: Set[String] =
    Set(
      "config.version",
      "syntax.highlighting"
    ) ++
      EditorConfig.Schema.currentKeys ++
      LanguageToolsConfig.Schema.currentKeys ++
      SurfaceConfig.Schema.currentKeys ++
      CursorConfig.Schema.currentKeys ++
      DocumentConfig.Schema.currentKeys ++
      InterfaceConfig.Schema.currentKeys ++
      WindowConfig.Schema.currentKeys ++
      InputConfig.Schema.currentKeys ++
      Set(
        "window.sitter.enabled",
        "window.sitter.action",
        "window.sitter.frames",
        "window.sitter.active_ticks",
        "window.sitter.fast_active_ticks",
        "window.sitter.fast_typing_threshold_ms"
      )

  val deprecatedKeys: Map[String, String] =
    Map(
      "syntax_highlighting" -> "syntax.highlighting"
    ) ++
      EditorConfig.Schema.deprecatedKeys ++
      LanguageToolsConfig.Schema.deprecatedKeys ++
      SurfaceConfig.Schema.deprecatedKeys ++
      CursorConfig.Schema.deprecatedKeys ++
      DocumentConfig.Schema.deprecatedKeys ++
      InterfaceConfig.Schema.deprecatedKeys ++
      WindowConfig.Schema.deprecatedKeys
