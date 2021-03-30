name := "mapk"
organization := "com.codedx"
version := "1.0.0-SNAPSHOT"

scalaVersion := "2.12.13"
crossScalaVersions := List("2.12.13", "2.13.5")
scalacOptions := List("-deprecation", "-unchecked", "-feature", "-language:higherKinds")
scalacOptions ++= (scalaBinaryVersion.value match {
	case "2.12" => Seq("-Ypartial-unification")
	case _ => Nil
})

libraryDependencies += "org.typelevel" %% "cats-core" % "2.4.2"

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.3" cross CrossVersion.full)