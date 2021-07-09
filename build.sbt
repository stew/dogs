import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import ReleaseTransformations._

val catsVersion = "2.6.1"
val catsTestkitScalatestVersion = "2.1.5"
val scalacheckVersion = "1.15.4"
val algebraVersion = "2.2.3"
val Scala212 = "2.12.14"
val Scala213 = "2.13.6"
val Scala3 = "3.0.1"
val CrossVersions = Seq(Scala212, Scala213, Scala3)

lazy val buildSettings = Seq(
  Global / organization := "org.typelevel",
  Global / scalaVersion := Scala212,
  crossScalaVersions := CrossVersions
)

ThisBuild / crossScalaVersions := Seq(Scala212, Scala213, Scala3)
ThisBuild / scalaVersion := Scala212
ThisBuild / githubWorkflowPublishTargetBranches := Seq()
ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8", "adopt@1.11", "adopt@1.15")
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixAdditions +=
  "ci" -> List("validateJS", "validateJVM")
ThisBuild / githubWorkflowBuild := Seq(WorkflowStep.Sbt(List("${{ matrix.ci }}"), name = Some("Validation")))
ThisBuild / githubWorkflowAddedJobs ++= Seq(
  WorkflowJob(
    "coverage",
    "Coverage",
    githubWorkflowJobSetup.value.toList ::: List(
      WorkflowStep.Use(UseRef.Public("actions", "setup-python", "v2"), name = Some("Setup Python")),
      WorkflowStep.Run(List("pip install codecov"), name = Some("Install Codecov")),
      WorkflowStep
        .Sbt(List("coverage", "testsJVM/test", "testsJVM/coverageReport"), name = Some("Calculate test coverage")),
      WorkflowStep.Run(List("codecov"), name = Some("Upload coverage results"))
    ),
    scalas = List(Scala212)
  ),
  WorkflowJob(
    "microsite",
    "Microsite",
    githubWorkflowJobSetup.value.toList ::: List(
      WorkflowStep.Use(
        UseRef.Public("ruby", "setup-ruby", "v1"),
        name = Some("Setup Ruby"),
        params = Map("ruby-version" -> "2.6", "bundler-cache" -> "true")
      ),
      WorkflowStep.Run(List("gem install jekyll -v 2.5"), name = Some("Install Jekyll")),
      WorkflowStep.Sbt(List("docs/clean"), name = Some("Clean microsite")),
      WorkflowStep.Sbt(List("docs/makeMicrosite"), name = Some("Build microsite"))
    ),
    scalas = List(Scala213)
  )
)

lazy val `cats-collections` = project.in(file("."))
  .settings(buildSettings:_*)
  .settings(noPublishSettings)
  .aggregate(coreJVM, coreJS, bench, scalacheckJVM, scalacheckJS, testsJVM, testsJS, docs, lawsJVM, lawsJS)
  .settings(
    releaseCrossBuild := true,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      publishArtifacts,
      releaseStepCommand("sonatypeReleaseAll"),
      setNextVersion,
      commitNextVersion,
      pushChanges))

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(moduleName := "cats-collections-core")
  .settings(dogsSettings:_*)
  .settings(publishSettings)
  .settings(
    Compile / unmanagedSourceDirectories ++= {
      val bd = baseDirectory.value
      def extraDirs(suffix: String) =
        CrossType.Pure.sharedSrcDir(bd, "main").toList map (f => file(f.getPath + suffix))
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, y)) if y <= 12 =>
          extraDirs("-2.12-")
        case Some((2, y)) if y >= 13 =>
          extraDirs("-2.13+")
        case Some((3, _)) =>
          extraDirs("-2.13+")
        case _ => Nil
      }
    }
  )
  .jsSettings(coverageEnabled := false)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val scalacheck = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core)
  .settings(moduleName := "cats-collections-scalacheck")
  .settings(dogsSettings:_*)
  .settings(publishSettings)
  .settings(
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalacheckVersion
  )
  .jsSettings(coverageEnabled := false)

lazy val scalacheckJVM = scalacheck.jvm
lazy val scalacheckJS = scalacheck.js

lazy val laws = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(core)
  .settings(moduleName := "cats-collections-laws")
  .settings(dogsSettings:_*)
  .settings(publishSettings)
  .settings(
    libraryDependencies += "org.typelevel" %%% "cats-laws" % catsVersion
  )
  .jsSettings(coverageEnabled := false)

lazy val lawsJVM = laws.jvm
lazy val lawsJS = laws.js

lazy val tests = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(scalacheck, laws)
  .settings(moduleName := "cats-collections-tests")
  .settings(dogsSettings:_*)
  .settings(noPublishSettings)
  .settings(coverageEnabled := false,
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-minSuccessfulTests", "1000"), // "-verbosity", "2"), // increase for stress tests
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-laws"              % catsVersion                 % "test",
      "org.typelevel" %%% "algebra-laws"           % algebraVersion              % "test",
      "org.typelevel" %%% "cats-testkit-scalatest" % catsTestkitScalatestVersion % "test"
    ),
    buildInfoPackage := "cats.collections",
    buildInfoKeys := Seq("isJvm" -> (crossProjectPlatform.value == JVMPlatform))
  )

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

lazy val docs = project
  .enablePlugins(MdocPlugin)
  .enablePlugins(MicrositesPlugin)
  .dependsOn(coreJVM)
  .settings(dogsSettings:_*)
  .settings(noPublishSettings)

lazy val bench = project
  .settings(moduleName := "cats-collections-bench")
  .dependsOn(coreJVM)
  .settings(noPublishSettings)
  .settings(
    buildSettings,
    coverageEnabled := false,
    run / fork := true,
    libraryDependencies += {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) => "org.scalaz" %% "scalaz-core" % "7.4.0-M7"
        case _            => "org.scalaz" %% "scalaz-core" % "7.3.3"
      }
    }
  )
  .enablePlugins(JmhPlugin)

lazy val dogsSettings = buildSettings ++ commonSettings ++ scoverageSettings

lazy val commonSettings =
  compilerFlags ++ Seq(
    libraryDependencies ++= {
      val deps = Seq(
        "org.typelevel" %%% "cats-core" % catsVersion,
        "org.typelevel" %%% "algebra"   % algebraVersion,
      )
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          deps
        case _ =>
          deps :+ compilerPlugin("org.typelevel"  %% "kind-projector" % "0.13.0" cross CrossVersion.full)
      }
    },
    test / fork := true
  )

addCommandAlias("validateJVM", ";testsJVM/scalastyle;testsJVM/compile;testsJVM/test")
addCommandAlias("validateJS", ";testsJS/compile;testsJS/test")

lazy val scoverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := false,
  coverageHighlighting := scalaBinaryVersion.value != "2.11"
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

lazy val publishSettings = Seq(
  ThisBuild / publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  publishMavenStyle := true,
  Test / publishArtifact := false,
  homepage := Some(url("https://github.com/typelevel/cats-collections")),
  pomIncludeRepository := Function.const(false),
  licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT-"),
                  "BSD-3" -> url("https://opensource.org/licenses/BSD-3-Clause")),
  scmInfo := Some(ScmInfo(url("https://github.com/typelevel/cats-collections"), "scm:git:git@github.com:typelevel/cats-collections.git")),
  autoAPIMappings := true,
  releaseProcess := Nil,
  pomExtra := (
    <developers>
      <developer>
        <id>anicolaspp</id>
        <name>Nicolas A Perez</name>
        <url>https://github.com/anicolaspp/</url>
      </developer>
    </developers>
  )
)

lazy val compilerFlags = Seq(
  scalacOptions ++= (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 =>
        Seq(
          "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
          "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
          "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
          "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
          "-Xlint:unsound-match",              // Pattern match may not be typesafe.
          "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
          "-Ypartial-unification"              // Enable partial unification in type constructor inference
        )
      case _ =>
        Seq()
    }
  ),
  scalacOptions ++= (
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) =>
        Seq(
          "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
          "-encoding", "utf-8",                // Specify character encoding used by source files.
          "-explain-types",                    // Explain type errors in more detail.
          "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
          "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
          "-language:higherKinds",             // Allow higher-kinded types
          "-language:implicitConversions",     // Allow definition of implicit functions called views
          "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
          "-Ykind-projector",
      )
      case Some((2, n)) if n <= 11 => // for 2.11 all we care about is capabilities, not warnings
        Seq(
          "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
          "-language:higherKinds",             // Allow higher-kinded types
          "-language:implicitConversions",     // Allow definition of implicit functions called views
        )
      case _ =>
        Seq(
          "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
          "-encoding", "utf-8",                // Specify character encoding used by source files.
          "-explaintypes",                     // Explain type errors in more detail.
          "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
          "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
          "-language:higherKinds",             // Allow higher-kinded types
          "-language:implicitConversions",     // Allow definition of implicit functions called views
          "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
          "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
          "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
          "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
          "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
          "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
          "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
          "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
          "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
          "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
          "-Xlint:option-implicit",            // Option.apply used implicit view.
          "-Xlint:package-object-classes",     // Class or object defined in package object.
          "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
          "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
          "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
          "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
          "-Ywarn-dead-code",                  // Warn when dead code is identified.
          "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
          "-Ywarn-numeric-widen",              // Warn when numerics are widened.
          "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
          "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
          "-Ywarn-unused:locals",              // Warn if a local definition is unused.
          "-Ywarn-unused:params",              // Warn if a value parameter is unused.
          "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
          "-Ywarn-unused:privates",            // Warn if a private member is unused.
          "-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
          "-Yrangepos"                         // Syntax highlighting for whole error range.
        )
    }
  ),
  Test / compile / scalacOptions    -= "-deprecation", // 2.13.4 collections
  Compile / console / scalacOptions -= "-Ywarn-unused:imports",
  Compile / doc / scalacOptions     -= "-Ywarn-unused:imports"
)
