name := "trading-platform"

version := "0.1"

scalaVersion := "2.12.6"


enablePlugins(JmhPlugin)

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-log4j12" % "1.7.25",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.8.0"
)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0"

libraryDependencies += "nl.grons" %% "metrics-scala-hdr" % "4.0.0"

libraryDependencies += "org.openjdk.jmh" % "jmh-core" % "1.21"
libraryDependencies += "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.21" % "provided"

