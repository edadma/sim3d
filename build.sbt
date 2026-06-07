import xerial.sbt.Sonatype.sonatypeCentralHost

ThisBuild / licenses               := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.8.4"
ThisBuild / organization           := "io.github.edadma"
ThisBuild / organizationName       := "edadma"
ThisBuild / organizationHomepage   := Some(url("https://github.com/edadma"))
ThisBuild / version                := "0.0.1"
ThisBuild / sonatypeCredentialHost := sonatypeCentralHost

ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true).withChecksums(Vector.empty)
ThisBuild / resolvers += Resolver.mavenLocal
ThisBuild / resolvers += Resolver.sonatypeCentralSnapshots
ThisBuild / resolvers += Resolver.sonatypeCentralRepo("releases")

ThisBuild / sonatypeProfileName := "io.github.edadma"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/edadma/sim3d"),
    "scm:git@github.com:edadma/sim3d.git",
  ),
)
ThisBuild / developers := List(
  Developer(
    id = "edadma",
    name = "Edward A. Maxedon, Sr.",
    email = "edadma@gmail.com",
    url = url("https://github.com/edadma"),
  ),
)

ThisBuild / homepage := Some(url("https://github.com/edadma/sim3d"))
ThisBuild / description := "A pluggable, graphics-agnostic 3D physics simulator with symplectic integrators (N-body gravity now, rigid bodies later)."

ThisBuild / publishTo := sonatypePublishToBundle.value

lazy val sim3d = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name := "sim3d",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics",
      ),
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
    publishMavenStyle      := true,
    Test / publishArtifact := false,
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
    // Fork so the Swing GUI runs in its own JVM and the window stays alive
    // (an unforked `sbt run` exits the JVM as soon as `main` returns).
    run / fork := true,
  )
  .nativeSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
    // SDL3 is the platform layer (window, input, present, texture upload); Cairo is the
    // drawing engine — anti-aliased fills/lines with no supersampling. scala-native finds the
    // Homebrew-installed libSDL3 / libcairo via each binding's `@link`.
    libraryDependencies += "io.github.edadma" %%% "sdl3" % "0.2.2",
    libraryDependencies += "io.github.edadma" %%% "libcairo" % "0.0.4",
  )
  .jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer        := true,
  )

lazy val root = project
  .in(file("."))
  .aggregate(sim3d.js, sim3d.jvm, sim3d.native)
  .settings(
    name                := "sim3d",
    publish / skip      := true,
    publishLocal / skip := true,
  )
