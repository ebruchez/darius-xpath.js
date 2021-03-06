enablePlugins(ScalaJSPlugin)

lazy val dariusXPath = (project in file(".")).
  settings(
    organization := "org.orbeon",
    name         := "darius-xpath",
    version      := "1.0-SNAPSHOT",

    scalaVersion := "2.11.7",

    scalaSource in Compile := baseDirectory.value / "xpath" / "src",
    scalaSource in Test    := baseDirectory.value / "xpath" / "test",

    jsDependencies      += RuntimeDOM,

    libraryDependencies += "org.scala-js" %%% "scalajs-dom"    % "0.8.1",
    libraryDependencies += "be.doeraene"  %%% "scalajs-jquery" % "0.8.0",
    libraryDependencies += "com.lihaoyi"  %%% "utest"          % "0.3.1" % "test",
    libraryDependencies += "com.lihaoyi"  %%% "scalarx"        % "0.2.8",
    libraryDependencies += "com.lihaoyi"  %%% "upickle"        % "0.2.8",
    libraryDependencies += "org.orbeon"   %%% "darius-xml"     % "1.0-SNAPSHOT",

    testFrameworks      += new TestFramework("utest.runner.Framework"),

    persistLauncher in Compile := true,
    persistLauncher in Test    := false

  )

val fastOptJSCombine = taskKey[File]("Combine -fastopt.js with -launcher.js.")
val fullOptJSCombine = taskKey[File]("Combine -opt.js with -launcher.js.")

def combine(optFile: File): File = {

  val outputDir = optFile.getParentFile

  val Format = "((.+)-(fastopt|opt)).js".r

  val combinedName = optFile.name match { case Format(name, _, _)   ⇒ s"$name-combined.js" }
  val launcherName = optFile.name match { case Format(_, prefix, _) ⇒ s"$prefix-launcher.js" }

  val combinedFile  = outputDir / combinedName
  val launcherFile  = outputDir / launcherName

  IO.delete(combinedFile)
  IO.append(combinedFile, IO.readBytes(optFile))
  IO.append(combinedFile, IO.readBytes(launcherFile))
  IO.append(combinedFile, "//# sourceMappingURL=darius-xpath-fastopt.js.map\n")


  combinedFile
}

fastOptJSCombine := combine((fastOptJS in Compile).value.data)
fullOptJSCombine := combine((fullOptJS in Compile).value.data)

sound.play(compile in Compile, Sounds.Blow, Sounds.Basso)
