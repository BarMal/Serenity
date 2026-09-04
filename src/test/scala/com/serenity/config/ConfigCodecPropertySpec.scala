package com.serenity.config

import java.nio.file.Files

import com.serenity.testkit.ConfigGenerators
import com.typesafe.config.ConfigFactory
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** The config file format, held to its own promise: whatever the settings are, saving and loading gets them back.
  *
  * `ConfigRoundTripSpec` pins the same properties against one hand-written fixture, which can only exercise the values
  * someone thought to write down -- and the writer's faults have been about *values* (an unquoted comma in a segment
  * list), not about which fields were touched. These properties push arbitrary values through the real file instead:
  * font families containing quotes, backslashes, `#` and `${...}`; every enum case; the full numeric ranges.
  *
  * Generated values sit inside their own clamps ([[com.serenity.testkit.ConfigGenerators]]), so the round trip is a
  * plain equality rather than one modulo normalisation.
  */
class ConfigCodecPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 60)

  private def savedText(config: AppConfig): String =
    val file = Files.createTempFile("serenity-config-property", ".conf")
    try
      ConfigManager.saveConfig(config, file) shouldBe true
      Files.readString(file)
    finally Files.deleteIfExists(file): Unit

  private def savedAndReloaded(config: AppConfig): AppConfig =
    val file = Files.createTempFile("serenity-config-property", ".conf")
    try
      ConfigManager.saveConfig(config, file) shouldBe true
      ConfigManager.loadConfig(Some(file.toString))
    finally Files.deleteIfExists(file): Unit

  private def writtenKeys(text: String): List[String] =
    text.linesIterator
      .map(_.trim)
      .filterNot(line => line.isEmpty || line.startsWith("#"))
      .flatMap(line => line.split("=", 2).headOption)
      .map(_.trim.stripPrefix("\"").stripSuffix("\""))
      .filter(_.matches("[A-Za-z0-9_.]+"))
      .toList

  private def differences(path: String, before: Any, after: Any): List[String] =
    (before, after) match
      case (b: Product, a: Product) if b.getClass == a.getClass && b.productArity > 0 =>
        b.productElementNames
          .zip(b.productIterator.zip(a.productIterator))
          .flatMap { case (name, (bv, av)) => differences(if path.isEmpty then name else s"$path.$name", bv, av) }
          .toList
      case (b, a) if b == a || notVaried.contains(path) => Nil
      case (b, a)                                       => List(s"$path: saved $b, loaded back $a")

  /** Fields a generated config deliberately holds at one value, with the reason. Everything else must vary, or the
    * properties below are only checking the fields someone remembered -- which is the blind spot a generator is
    * supposed to remove, not inherit.
    */
  private val notVaried: Set[String] = Set(
    // Legacy mirrors of the motion hierarchy, kept in the model for files written before it existed. A save writes the
    // hierarchy and a load restores it, leaving these at their defaults -- so what has to survive is the behaviour they
    // feed, which the effective-motion property below asserts directly.
    "surfaceConfig.elementTransitionSpeedScale",
    "surfaceConfig.editorInsertionTransitionKind",
    "surfaceConfig.commandRunnerTransitionKind",
    "surfaceConfig.panelOpenTransitionKind",
    "surfaceConfig.panelCloseTransitionKind",
    // Keyed maps with their own codecs, dynamic key prefixes and specs (see `ConfigGenerators`).
    "languageToolsConfig.lspUserConfig.servers",
    "inputConfig.hotkeyConfig.bindings",
    "inputConfig.hotkeyConfig.overrides",
    "inputConfig.focusedKeymapConfig.editor.bindings",
    "inputConfig.focusedKeymapConfig.commandRunner.bindings",
    "inputConfig.focusedKeymapConfig.modal.bindings",
    "inputConfig.focusedKeymapConfig.panel.bindings",
    "inputConfig.focusedKeymapConfig.peek.bindings"
  )

  /** Every field of the config tree, by path.
    *
    * An `Option` contributes twice: once as itself, so that "sometimes set, sometimes not" counts as variation, and
    * once through its contents, so the fields inside a `Some` are covered too. Without the first, a field that is
    * `None` in one sample and `Some(...)` in the next looks like it never varies -- the `Some` case recurses past the
    * path the `None` case reports.
    */
  private def leafValues(config: AppConfig): Map[String, Any] =
    def walk(path: String, value: Any): List[(String, Any)] =
      value match
        case option: Option[?] =>
          (path -> option) :: option.toList.flatMap(inner => walk(s"$path.value", inner))
        case product: Product if product.productArity > 0 && !product.isInstanceOf[Iterable[?]] =>
          product.productElementNames
            .zip(product.productIterator)
            .flatMap { case (name, element) => walk(if path.isEmpty then name else s"$path.$name", element) }
            .toList
        case other => List(path -> other)
    walk("", config).toMap

  "the configuration generator" should "vary every field, so the properties below cover all of them" in {
    val samples = Gen.listOfN(40, ConfigGenerators.genAppConfig).sample.getOrElse(Nil).map(leafValues)
    val paths   = samples.flatMap(_.keySet).distinct
    val constant = paths
      .filterNot(notVaried.contains)
      .filter(path => samples.flatMap(_.get(path)).distinct.sizeIs <= 1)
      .sorted

    withClue(
      s"fields the generator never varies, so nothing here checks that they survive a save and reload:\n${constant
          .mkString("\n")}\n"
    ) {
      constant shouldBe empty
    }
  }

  "any configuration" should "be written as a file this module can read" in
    forAll(ConfigGenerators.genAppConfig) { config =>
      noException should be thrownBy ConfigFactory.parseString(savedText(config))
    }

  it should "never write a key that is also a prefix of another key" in
    // HOCON resolves `a.b = value` alongside `a.b.c = value` by dropping the leaf, silently. Both of the format's
    // value-losing bugs were this shape, so the property is about the key set rather than about any one setting.
    forAll(ConfigGenerators.genAppConfig) { config =>
      val keys     = writtenKeys(savedText(config))
      val shadowed = keys.filter(key => keys.exists(other => other != key && other.startsWith(s"$key.")))
      withClue(s"keys that are also parents of another key: ${shadowed.mkString(", ")}\n") {
        shadowed shouldBe empty
      }
    }

  it should "keep the motion behaviour it was saved with" in
    forAll(ConfigGenerators.genAppConfig) { config =>
      val reloaded = savedAndReloaded(config)
      reloaded.surfaceConfig.effectiveMotionBaseline shouldBe config.surfaceConfig.effectiveMotionBaseline
      reloaded.surfaceConfig.effectiveCommandRunnerTransitionKind shouldBe
        config.surfaceConfig.effectiveCommandRunnerTransitionKind
      reloaded.surfaceConfig.effectivePanelOpenTransitionKind shouldBe
        config.surfaceConfig.effectivePanelOpenTransitionKind
      reloaded.surfaceConfig.effectivePanelCloseTransitionKind shouldBe
        config.surfaceConfig.effectivePanelCloseTransitionKind
      // Per family, from the hierarchy the renderer reads. Not `effectiveEditorTextTransitionSpeedScale` and friends:
      // those resolve `editorTextTransitionSpeedScale.getOrElse(elementTransitionSpeedScale)` without consulting the
      // hierarchy at all, so they answer from the legacy fields a saved file no longer carries even when the hierarchy
      // holds the same value.
      def familySpeeds(candidate: AppConfig): Map[MotionFamily, Double] =
        candidate.surfaceConfig.effectiveMotionConfiguration.families.view.mapValues(_.speedScale).toMap

      familySpeeds(reloaded) shouldBe familySpeeds(config)
    }

  it should "come back exactly as it was saved" in
    forAll(ConfigGenerators.genAppConfig) { config =>
      val lost = differences("", config, savedAndReloaded(config))
      withClue(s"${lost.size} field(s) lost:\n${lost.mkString("\n")}\n")(lost shouldBe empty)
    }
