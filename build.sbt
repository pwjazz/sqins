name := "sqins"

version := "1.0"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.6.1" % "test",
  "postgresql" % "postgresql" % "9.1-901.jdbc4"
)

resolvers ++= Seq(
  "Maven Repository" at "http://mvnrepository.com/"
)