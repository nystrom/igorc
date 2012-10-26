name := "igor-compiler"

version := "0.1.0"

organization := "ch.usi"

scalaVersion := "2.9.2"

sourceDirectories in Compile := Seq(file("src"))

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.8" % "test->default",
  "org.scalatest" %% "scalatest" % "1.6.1" % "test",
  "asm" % "asm-all" % "3.3.1",
  "com.googlecode.kiama" %% "kiama" % "1.2.0"
)
