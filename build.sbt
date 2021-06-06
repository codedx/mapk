name := "mapk"
version := "1.2.0"

scalaVersion := "2.12.14"
crossScalaVersions := List("2.12.14", "2.13.6", "3.0.0")

libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
libraryDependencies ++= (scalaBinaryVersion.value match {
	case "2.12" | "2.13" => compilerPlugin("org.typelevel" % "kind-projector" % "0.13.0" cross CrossVersion.full) :: Nil
	case _ => Nil
})

scalacOptions := List("-deprecation", "-unchecked", "-feature", "-language:higherKinds")
scalacOptions ++= {
	CrossVersion.partialVersion(scalaVersion.value) match {
		case Some((2, 12)) => Seq("-Ypartial-unification", "-Xsource:3")
		case Some((2, 13)) => Seq("-Xsource:3")
		case Some((3, _)) => Seq("-Ykind-projector")
		case _ => Nil
	}
}
