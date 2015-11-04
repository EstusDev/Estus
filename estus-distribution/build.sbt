name := "estus-distribution"

version := "0.0.0"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4-SNAPSHOT",
  "com.typesafe.akka" %% "akka-testkit" % "2.4-SNAPSHOT",
  "org.apache.spark" %% "spark-core" % "1.4.1",
  "org.apache.commons" % "commons-math3" % "3.5",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)
