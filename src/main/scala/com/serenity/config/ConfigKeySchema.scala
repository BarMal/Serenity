package com.serenity.config

/** Which keys the text config format understands, derived from the settings themselves.
  *
  * This used to be a hand-maintained list beside the parser's own, and the two had drifted: eleven spellings the parser
  * accepted were reported to the user as unknown keys. Reading both off [[ConfigRegistry]] and [[ConfigGroups]] is what
  * stops that happening again -- a key is known exactly when something can read it.
  */
object ConfigKeySchema:

  val dynamicPrefixes: List[String] = ConfigGroups.dynamicPrefixes

  val currentKeys: Set[String] =
    Set("config.version") ++ ConfigRegistry.writtenKeys ++ ConfigGroups.currentKeys

  /** An old spelling and the current one to use instead.
    *
    * Most come straight from the fields' own aliases. The exceptions are the spellings that set more than one field, so
    * there is no single key to point at.
    */
  val deprecatedKeys: Map[String, String] =
    ConfigRegistry.fields.flatMap { field =>
      field.aliases.filterNot(currentKeys.contains).map(_ -> field.key)
    }.toMap ++ ConfigGroups.deprecatedKeys ++ ConfigLegacyKeys.replacements

  def deprecatedReplacement(key: String): Option[String] =
    deprecatedKeys.get(key)

  def isKnownKey(key: String): Boolean =
    currentKeys.contains(key) ||
      deprecatedKeys.contains(key) ||
      ConfigRegistry.allKeys.contains(key) ||
      ConfigLegacyKeys.handles(key) ||
      ConfigGroups.handles(key) ||
      dynamicPrefixes.exists(key.startsWith)
