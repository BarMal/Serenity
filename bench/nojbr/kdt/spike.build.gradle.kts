// Throwaway module added to a clone of JetBrains/kotlin-desktop-toolkit by the CI job (#2380, never merge).
import org.jetbrains.desktop.buildscripts.KotlinDesktopToolkitArtifactType
import org.jetbrains.desktop.buildscripts.KotlinDesktopToolkitAttributes
import org.jetbrains.desktop.buildscripts.KotlinDesktopToolkitNativeProfile

plugins {
    kotlin("jvm")
}

repositories {
    mavenCentral()
}

dependencies {
    implementation(project(":kotlin-desktop-toolkit"))
    implementation(kotlin("stdlib"))
}

java {
    toolchain {
        languageVersion = JavaLanguageVersion.of(25)
    }
}

val wantedProfile: KotlinDesktopToolkitNativeProfile =
    runCatching { KotlinDesktopToolkitNativeProfile.valueOf((findProperty("kdt.profile") as String? ?: "RELEASE").uppercase()) }
        .getOrElse { KotlinDesktopToolkitNativeProfile.DEBUG }

val depScope = configurations.dependencyScope("native") {
    withDependencies {
        add(project.dependencies.project(":kotlin-desktop-toolkit"))
    }
}
val nativeLib = configurations.resolvable("nativeParts") {
    extendsFrom(depScope.get())
    attributes {
        attribute(KotlinDesktopToolkitAttributes.TYPE, KotlinDesktopToolkitArtifactType.NATIVE_LIBRARY)
        attribute(KotlinDesktopToolkitAttributes.PROFILE, wantedProfile)
    }
}

tasks.register("printKdtPaths") {
    val cp = sourceSets["main"].runtimeClasspath
    inputs.files(cp)
    inputs.files(nativeLib)
    doLast {
        println("KDT_PROFILE=$wantedProfile")
        println("KDT_CP=" + cp.files.joinToString(":"))
        println("KDT_NATIVE=" + nativeLib.get().files.joinToString(":"))
    }
}
