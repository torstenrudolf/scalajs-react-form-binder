
def commonSettings: Project => Project =
  _.enablePlugins(ScalaJSPlugin)
    .settings(
      organization := "com.github.torstenrudolf.scalajs-react-form-binder",
      version := "0.0.17-SNAPSHOT",
      homepage := Some(url("https://github.com/torstenrudolf/scalajs-react-form-binder")),
      licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
      scalaVersion := "2.11.8",
      publishTo <<= version { (v: String) =>
        val nexus = "https://oss.sonatype.org/"
        if (v.trim.endsWith("SNAPSHOT"))
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases" at nexus + "service/local/staging/deploy/maven2")
      },
      publishArtifact in Test := false,
      pomExtra :=
        <url>https://github.com/torstenrudolf/scalajs-react-form-binder</url>
          <licenses>
            <license>
              <name>MIT license</name>
              <url>http://www.opensource.org/licenses/mit-license.php</url>
            </license>
          </licenses>
          <scm>
            <url>git://github.com/torstenrudolf/scalajs-react-form-binder.git</url>
            <connection>scm:git://github.com/torstenrudolf/scalajs-react-form-binder.git</connection>
          </scm>
          <developers>
            <developer>
              <id>torstenrudolf</id>
              <name>Torsten Rudolf</name>
              <url>https://github.com/torstenrudolf</url>
            </developer>
          </developers>
    )

def preventPublication: Project => Project =
  _.settings(
    publishTo := Some(Resolver.file("Unused transient repository", target.value / "fakepublish")),
    publishArtifact := false)


lazy val core = project
  .configure(commonSettings)
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "com.github.japgolly.scalajs-react" %%% "extra" % "0.11.3"
    ),
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.4.5" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework")
//    scalacOptions += "-Ymacro-debug-lite"
  )

lazy val extras = project
  .dependsOn(core)
  .configure(commonSettings)
  .settings(
    name := "extras",
    libraryDependencies += "com.github.chandu0101.scalajs-react-components" %%% "core" % "0.5.0"
  )


lazy val installExternalNPMDeps = TaskKey[Unit]("Execute the npm build command to build the external ui dependencies")
def filesToWatchForExternalNPMDepsTask(workingDir: File) : Seq[File] = {
  Seq(workingDir / "package.json",
    workingDir / "webpack.config.js",
    workingDir / "webpack.config.prod.js",
    workingDir / "resources/jsBundles/index.js")
}
def installExternalNPMDeps(workingDir: File): Unit = {
  // use FileFunction.cached to run this task only if the package.json or webpack files have changed
  FileFunction.cached(workingDir / "build/cache",
    FilesInfo.lastModified, /* inStyle */
    FilesInfo.exists) /* outStyle */ {
    (inFiles: Set[File]) => {
      val installCommand = Seq("npm", "install")
      println(s"${installCommand.mkString(" ")}")
      Process(installCommand, workingDir) !!

      val runCommand = Seq("npm", "run", "build")
      println(s"${runCommand.mkString(" ")}")
      Process(runCommand, workingDir) !!

      Set(workingDir / "build/frontend-jsdeps.js")
    }
  }(filesToWatchForExternalNPMDepsTask(workingDir).toSet)

}

lazy val demo = project
  .dependsOn(extras)
  .configure(commonSettings, preventPublication)
  .settings(
    name := "demo",
    libraryDependencies ++= Seq(
      "com.github.chandu0101.scalajs-react-components" %%% "core" % "0.5.0"
    ),
    Seq(packageScalaJSLauncher, fastOptJS, fullOptJS) map { packageJSKey =>
      crossTarget in(Compile, packageJSKey) := baseDirectory.value / "build"
    },
    installExternalNPMDeps := installExternalNPMDeps(baseDirectory.value),
    watchSources <++= baseDirectory map { path => filesToWatchForExternalNPMDepsTask(path)},
    compile in Compile <<= (compile in Compile) dependsOn installExternalNPMDeps,
    cleanFiles <++= baseDirectory { base => Seq(base / "build", base / "node_modules") }
  )


lazy val root = (project in file("."))
  .aggregate(core, extras, demo)
  .configure(commonSettings, preventPublication)
