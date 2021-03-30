# MapK

`MapK` is a utility class that we implemented for Code Dx. It's like a `scala.collection.immutable.Map[K, V]`, but where `K` and `V` are "higher-kinded" types like `Option` or `List`.

It's useful for when you have some "key" class that's parameterized on a type `T`, and you want to store a value of the appropriate type for each key, e.g.

```scala
import com.codedx.util.MapK

abstract class MyKey[T](val name: String)
case object Age extends MyKey[Int]("age")
case object Name extends MyKey[String]("name")
case object NumThings extends MyKey[Int]("numThings")

val info: MapK[MyKey, cats.Id] = MapK.empty[MyKey, cats.Id]
   .updated(Age, 21)
   .updated(Name, "Dylan")

val age: Option[Int] = info.get(Age) // Some(21)
val name: Option[String] = info.get(Name) // Some("Dylan")
val numThings: Option[Int] = info.get(NumThings) // None
```

# Install

In SBT:

```scala
resolvers += "GitHub Package Registry (codedx)" at "https://maven.pkg.github.com/codedx/_"
libraryDependencies += "com.codedx" %% "mapk" % "1.0.0"
```

Note that if you are using the `sbt-github-packages` plugin, you can replace the `resolvers` line with
`resolvers += Resolver.githubPackages("codedx")`

# Developing

This project uses SBT, plus the [`sbt-github-packages`](https://github.com/djspiewak/sbt-github-packages) Plugin for publishing packages to Github.

The packaging plugin requires some extra configuration in the form of a `GITHUB_TOKEN` environment variable.
Failing to set this seems to cause SBT/IntelliJ to freak out; IntelliJ won't import the project, and SBT's `update` seems to fail.
As long as you provide a token, everything seems to be ok. 
That said, I'm not sure what would happen if you aren't part of the `codedx` organization.
