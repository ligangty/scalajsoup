organization := "com.github.ligangty.scala"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.4"

name := "scalalearn"

//resolvers ++= Seq(
//  "repo.codahale.com" at "http://repo.codahale.com",
//  "akka" at "http://repo.akka.io/snapshots"
//)

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-lang3" % "3.3.1",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "org.slf4j" % "slf4j-log4j12" % "1.7.5",
  "org.slf4j" % "slf4j-simple" % "1.7.5",
  "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test"
)