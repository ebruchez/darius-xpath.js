enablePlugins(ScalaJSPlugin)

name         := "Saxon-CE-Scala"
version      := "1.0"
scalaVersion := "2.11.6"

scalaSource in Compile := baseDirectory.value / "xpath" / "src"
scalaSource in Test    := baseDirectory.value / "xpath" / "test"

jsDependencies      += RuntimeDOM

libraryDependencies += "org.scala-js" %%% "scalajs-dom"    % "0.8.0"
libraryDependencies += "be.doeraene"  %%% "scalajs-jquery" % "0.8.0"
libraryDependencies += "com.lihaoyi"  %%% "utest"          % "0.3.0" % "test"
libraryDependencies += "com.lihaoyi"  %%% "scalarx"        % "0.2.8"

testFrameworks      += new TestFramework("utest.runner.Framework")

persistLauncher in Compile := true
persistLauncher in Test    := false

sound.play(compile in Compile, Sounds.Blow, Sounds.Basso)