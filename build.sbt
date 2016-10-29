
def commonSettings: Project => Project =
  _.enablePlugins(ScalaJSPlugin)
    .settings(
      organization := "com.github.torstenrudolf.scalajs-react-form-binder",
      version := "0.0.1-SNAPSHOT",
      homepage := Some(url("https://github.com/torstenrudolf/scalajs-react-form-binder")),
      licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
      scalaVersion := "2.11.8")

def preventPublication: Project => Project =
  _.settings(
    publishTo := Some(Resolver.file("Unused transient repository", target.value / "fakepublish")),
    publishArtifact := false)


lazy val core = (project in file("core")).configure(commonSettings)
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "com.github.japgolly.scalajs-react" %%% "extra" % "0.11.2"
    ),
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

lazy val root = (project in file("."))
  .aggregate(core)
  .configure(commonSettings, preventPublication)



