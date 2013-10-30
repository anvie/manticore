organization := "com.ansvia.manticore"

name := "manticore"

version := "0.1"

description := ""

scalaVersion := "2.9.2"

seq(ProguardPlugin.proguardSettings :_*)

proguardOptions += keepMain("com.ansvia.manticore.Manticore")

proguardOptions ++= Seq(
    "-keep class ch.qos.logback.*",
    "-keep class org.slf4j.*"
)

resolvers ++= Seq(
	"Sonatype Releases" at "https://oss.sonatype.org/content/groups/scala-tools",
	"typesafe repo"   at "http://repo.typesafe.com/typesafe/releases",
	"Ansvia release repo" at "http://scala.repo.ansvia.com/releases",
	"Ansvia snapshot repo" at "http://scala.repo.ansvia.com/nexus/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
    "org.specs2" % "specs2_2.9.2" % "1.12.4.1",
    "ch.qos.logback" % "logback-classic" % "1.0.13",
    "com.ansvia" % "ansvia-commons" % "0.0.9-20131015-SNAPSHOT",
    "commons-io" % "commons-io" % "20030203.000550"
)

EclipseKeys.withSource := true


publishTo <<= version { (v:String) =>
    val ansviaRepo = "http://scala.repo.ansvia.com/nexus"
    if(v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at ansviaRepo + "/content/repositories/snapshots")
    else
        Some("releases" at ansviaRepo + "/content/repositories/releases")
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

crossPaths := false

pomExtra := (
  <url>http://ansvia.com</url>
  <developers>
    <developer>
      <id>anvie</id>
      <name>Robin Sy</name>
      <url>http://www.mindtalk.com/u/robin</url>
    </developer>
  </developers>)

