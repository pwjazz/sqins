name := "sqins"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.6.1" % "test",
  "postgresql" % "postgresql" % "9.1-901.jdbc4"
)

resolvers ++= Seq(
  "Maven Repository" at "http://mvnrepository.com/"
)

publishTo := Some(Resolver.file("file",  new File( "/Users/percy/maven_repo/sqins-mvn-repo/releases" )) )

publishTo <<= version { (v: String) =>
  val base = "/Users/percy/maven_repo/sqins-mvn-repo/"
  if (v.trim.endsWith("SNAPSHOT")) 
    Some(Resolver.file("file",  new File( base + "snapshots" )) ) 
  else
    Some(Resolver.file("file",  new File( base + "releases" )) )
}