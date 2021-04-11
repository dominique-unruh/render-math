name := "render-math"

version := "0.1"

scalaVersion := "2.13.5"

idePackagePrefix := Some("de.unruh.rendermath")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % "test"
libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.2.5"
// https://mvnrepository.com/artifact/org.scilab.forge/jlatexmath
libraryDependencies += "org.scilab.forge" % "jlatexmath" % "1.0.7"
libraryDependencies += "org.jetbrains" % "annotations" % "20.1.0"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2"
