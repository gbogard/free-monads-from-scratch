ThisBuild / scalaVersion := "2.13.2"

val globalSettings = List(
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
  libraryDependencies ++=
    List(
      "org.typelevel" %% "cats-core" % "2.2.0-RC4",
      "org.typelevel" %% "cats-free" % "2.2.0-RC4",
    )
)


lazy val functorsCoproduct = (project in file("functors-coproduct")).settings(globalSettings)
lazy val freeMonads = (project in file("free-monads")).settings(globalSettings)
lazy val catsFreeMonads = (project in file("cats-free-monads")).settings(globalSettings)
