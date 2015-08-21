name := "GrammarComparison"

version := "0.1"

scalaVersion := "2.11.2"

organization := "ch.epfl.lara"

mainClass in (Compile, run) := Some("engine.Main")

mainClass in oneJar := Some("engine.Main")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

javacOptions += "-Xlint:unchecked"

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

libraryDependencies +=  "org.scalatest" % "scalatest_2.11" % "2.2.1"
 
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

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
    ("target/scala-2.11", "cfgchecker_2.11-0.1.jar", "lib/")
  ) map { p => (inDir / p._1 / p._2, outDir / p._3 / p._2) }
  IO.copy(files, true)
}
