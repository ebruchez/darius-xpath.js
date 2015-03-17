enablePlugins(ScalaJSPlugin)

name := "Saxon-CE-Scala"

version := "1.0"

scalaVersion := "2.11.6"

scalaSource in Compile := baseDirectory.value / "xpath" / "src"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0"

libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.8.0"