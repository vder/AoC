val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "co.fs2" %% "fs2-core" % "3.2.2",
    libraryDependencies += "co.fs2" %% "fs2-io" % "3.2.2",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.0",
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.6",
    scalacOptions += "-source:future"
  )
