name := "sqins"

organization := "org.sqins"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.1"

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

licenses := Seq("BSD-style" -> url("https://github.com/pwjazz/sqins/blob/master/LICENSE.txt"))

homepage := Some(url("http://www.sqins.org"))

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.6.1" % "test",
  "postgresql" % "postgresql" % "9.1-901.jdbc4" % "test"
)

resolvers ++= Seq(
  "Maven Repository" at "http://mvnrepository.com/"
)

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) 
    Some("snapshots" at nexus + "content/repositories/snapshots") 
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomExtra := (
  <scm>
    <url>git@github.com:pwjazz/sqins.git</url>
    <connection>scm:git:git@github.com:pwjazz/scala-arm.git</connection>
  </scm>
  <developers>
    <developer>
      <id>pwjazz</id>
      <name>Percy Wegmann</name>
      <url>http://www.percywegmann.com</url>
    </developer>
  </developers>)