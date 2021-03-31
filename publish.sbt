ThisBuild / organization := "com.codedx"
ThisBuild / organizationName := "Code Dx"
ThisBuild / organizationHomepage := Some(url("https://codedx.com/"))

ThisBuild / scmInfo := Some(
	ScmInfo(
		url("https://github.com/codedx/mapk"),
		"scm:git@github.com:codedx/mapk.git"
	)
)
ThisBuild / developers := List(
	Developer(
		id    = "codedx",
		name  = "Code Dx",
		email = "support@codedx.com",
		url   = url("https://codedx.com/")
	)
)

ThisBuild / description := "Map-like class for higher-kind key and value types"
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/codedx/mapk"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
	val nexus = "https://oss.sonatype.org/"
	if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
	else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true