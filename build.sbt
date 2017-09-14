name := "GrammarComparison"

version := "0.1"

scalaVersion := "2.12.3"

organization := "ch.epfl.lara"

mainClass in (Compile, run) := Some("grammarcomp.engine.Main")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

javacOptions += "-Xlint:unchecked"

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"

libraryDependencies += "org.antlr" % "antlr4" % "4.7"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

fork in run := true

unmanagedClasspath in Compile += baseDirectory.value / "bin"

unmanagedClasspath in Runtime += baseDirectory.value / "bin"

javaOptions += "-Xmx10G"

javaOptions += "-Xss20m"

javaOptions += "-Xms3G"

parallelExecution in Test := false

lazy val export = TaskKey[Unit]("export", "Copy the jar file to grammar-web")

export := {
  val outDir = baseDirectory.value / "../grammar-web"
  val inDir = baseDirectory.value
  val files = Seq(
    ("target/scala-2.12", "cfgchecker_2.12-0.1.jar", "lib/")
  ) map { p => (inDir / p._1 / p._2, outDir / p._3 / p._2) }
  IO.copy(files, true)
}

assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false)

