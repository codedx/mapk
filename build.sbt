name := "mapk"
version := "1.0.0"

scalaVersion := "2.12.13"
crossScalaVersions := List("2.12.13", "2.13.5")
scalacOptions := List("-deprecation", "-unchecked", "-feature", "-language:higherKinds")
scalacOptions ++= (scalaBinaryVersion.value match {
	case "2.12" => Seq("-Ypartial-unification")
	case _ => Nil
})

libraryDependencies += "org.typelevel" %% "cats-core" % "2.4.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test"

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.3" cross CrossVersion.full)

//