name := "render-math"

version := "0.1"

scalaVersion := "3.0.2"

idePackagePrefix := Some("de.unruh.rendermath")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.2.7"
// https://mvnrepository.com/artifact/org.scilab.forge/jlatexmath
libraryDependencies += "org.scilab.forge" % "jlatexmath" % "1.0.7"
libraryDependencies += "org.jetbrains" % "annotations" % "22.0.0"
//libraryDependencies += ("com.lihaoyi" % "fastparse" % "2.3.3") cross CrossVersion.for3Use2_13
// https://mvnrepository.com/artifact/org.log4s/log4s
libraryDependencies += "org.log4s" %% "log4s" % "1.10.0"
// https://mvnrepository.com/artifact/org.slf4j/slf4j-simple
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.32"
