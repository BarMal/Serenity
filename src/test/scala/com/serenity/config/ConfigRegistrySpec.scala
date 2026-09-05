package com.serenity.config

import com.serenity.testkit.ConfigGenerators
import com.typesafe.config.ConfigFactory
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** The registry is only worth having if it says the same thing the hand-written writer and parser said.
  *
  * These check that before anything is switched over to it, and go on checking it afterwards: the keys it claims, the
  * spellings it accepts, and -- the one that matters -- that a value written from a config reads back as that same
  * value.
  */
class ConfigRegistrySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  "ConfigRegistry" should "claim a key exactly once" in {
    val duplicated = ConfigRegistry.fields
      .flatMap(field => field.spellings.toList)
      .groupBy(identity)
      .collect { case (key, occurrences) if occurrences.sizeIs > 1 => key }

    duplicated shouldBe empty
  }

  it should "account for every key the config schema knows, and be the schema's only source" in {
    val schemaOnly = (ConfigKeySchema.currentKeys ++ ConfigKeySchema.deprecatedKeys.keySet)
      .filterNot(ConfigRegistry.allKeys.contains)
      .filterNot(ConfigGroups.handles)
      .filterNot(ConfigLegacyKeys.handles)
      .filterNot(_ == "config.version")

    schemaOnly shouldBe empty
    ConfigRegistry.allKeys.filterNot(ConfigKeySchema.isKnownKey) shouldBe empty
  }

  it should "write every key it claims to write" in {
    val written = ConfigFileFormat
      .render(AppConfig.default)
      .split("\n")
      .toList
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .flatMap(_.split("=", 2).headOption)
      .map(_.trim)
      .toSet

    ConfigRegistry.writtenKeys.filterNot(written.contains) shouldBe empty
  }

  it should "read back the value it wrote, for every field and every generated config" in {
    forAll(ConfigGenerators.genAppConfig) { config =>
      ConfigRegistry.fields.foreach { field =>
        withClue(s"${field.key}: ") {
          roundTrip(field, config) shouldBe Some(field.get(config))
        }
      }
    }
  }

  it should "read back the value it wrote under every spelling it accepts" in {
    forAll(Gen.oneOf(ConfigGenerators.genAppConfig, Gen.const(AppConfig.default))) { config =>
      ConfigRegistry.fields.foreach { field =>
        field.spellings.foreach { spelling =>
          withClue(s"$spelling: ") {
            ConfigRegistry.find(spelling) shouldBe Some(field)
          }
        }
      }
    }
  }

  it should "agree with the writer on what each setting's text is" in {
    forAll(ConfigGenerators.genAppConfig) { config =>
      val rendered = ConfigFileFormat
        .render(config)
        .split("\n")
        .toList
        .filter(line => line.nonEmpty && !line.startsWith("#"))
        .flatMap { line =>
          line.split("=", 2).toList match
            case key :: value :: Nil => Some(key.trim -> value.trim)
            case _                   => None
        }
        .toMap

      ConfigRegistry.fields.foreach { field =>
        withClue(s"${field.key}: ") {
          rendered.get(field.key) shouldBe Some(field.setting(config)._2.rendered)
        }
      }
    }
  }

  /** Through the text the file would carry and back through the parser, because the quoting is part of the trip. */
  private def roundTrip[A](field: ConfigField[A], config: AppConfig): Option[A] =
    val rendered = field.codec.render(field.get(config)).rendered
    field.codec.fromConfigValue(ConfigFactory.parseString(s"probe = $rendered").getValue("probe"))
