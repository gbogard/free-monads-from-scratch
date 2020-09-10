ThisBuild / scalaVersion := "2.13.2"

val globalSettings = List(
  addCompilerPlugin(
    "org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full
  ),
  testFrameworks += new TestFramework("munit.Framework"),
  libraryDependencies ++=
    List(
      "org.typelevel" %% "cats-core" % "2.2.0-RC4",
      "org.typelevel" %% "cats-free" % "2.2.0-RC4",
      "org.typelevel" %% "cats-effect" % "2.1.4",
      "org.scalameta" %% "munit" % "0.7.12" % Test
    )
)

lazy val alacarte =
  (project in file("data-types-a-la-carte")).settings(globalSettings)
lazy val catsFreeMonads =
  (project in file("cats-free-monads")).settings(globalSettings)
