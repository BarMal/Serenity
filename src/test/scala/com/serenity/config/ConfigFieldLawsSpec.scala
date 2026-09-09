package com.serenity.config

import com.serenity.testkit.ConfigGenerators
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** #1279 asked whether every [[ConfigField]] could be given a lawful `Monocle` lens. It cannot: several setters route
  * through a `normalized` that deliberately transforms the value on the way in (`PreferredWindowSize.normalized`
  * clamps, `SpellCheckConfig.normalized` trims/dedupes), so `get(set(s, a)) == a` is false by design and `LensLaws`
  * would fail on a meaningful fraction of fields for reasons that are features, not bugs.
  *
  * These are the laws that survive that, plus the one `ConfigField`'s own doc comment promises and the registry
  * actually depends on:
  *
  *   1. `set(s, get(s)) == s` -- putting a setting's own value back changes nothing.
  *   2. `set(set(s, a), a) == set(s, a)` -- normalisation settles rather than drifting under repeated application.
  *   3. Restoring a field leaves every other registered setting untouched -- "restoring assigns the field and nothing
  *      else", per [[ConfigField.decode]]'s doc comment.
  *
  * Law 3 is the one that has actually been violated: #1316 found it fabricating a window dimension nobody set, and law
  * 1 caught a material preset flipping on a value that had not changed. Both are fixed; this is the guard that keeps a
  * setter that reaches sideways a test failure instead of something to notice by hand.
  */
class ConfigFieldLawsSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 25)

  private def setOwnValue[A](field: ConfigField[A], config: AppConfig): AppConfig =
    field.set(config, field.get(config))

  private def settles[A](field: ConfigField[A], config: AppConfig, value: AppConfig): AppConfig =
    val a = field.get(value)
    field.set(field.set(config, a), a)

  private def setOnce[A](field: ConfigField[A], config: AppConfig, value: AppConfig): AppConfig =
    field.set(config, field.get(value))

  /** As reading a saved value back in does: the field's own encoding of `saved`, decoded into `config`. */
  private def restoreFrom[A](field: ConfigField[A], config: AppConfig, saved: AppConfig): AppConfig =
    field.decode(Json.obj(field.encode(saved)).hcursor, config)

  "Every registered ConfigField" should "leave the config unchanged when set to its own current value" in
    forAll(ConfigGenerators.genAppConfig) { config =>
      ConfigRegistry.fields.foreach { field =>
        withClue(s"${field.key}: ") {
          setOwnValue(field, config) shouldBe config
        }
      }
    }

  it should "settle: setting the same value twice is the same as setting it once" in
    forAll(ConfigGenerators.genAppConfig, ConfigGenerators.genAppConfig) { (config, other) =>
      ConfigRegistry.fields.foreach { field =>
        withClue(s"${field.key}: ") {
          settles(field, config, other) shouldBe setOnce(field, config, other)
        }
      }
    }

  it should "leave every other registered setting untouched when one is restored" in
    forAll(ConfigGenerators.genAppConfig, ConfigGenerators.genAppConfig) { (config, saved) =>
      ConfigRegistry.fields.foreach { field =>
        val restored = restoreFrom(field, config, saved)
        withClue(s"restoring ${field.key}: ") {
          ConfigRegistry.fields.filterNot(_.key == field.key).foreach { other =>
            withClue(s"moved ${other.key}: ") {
              other.get(restored) shouldBe other.get(config)
            }
          }
        }
      }
    }
