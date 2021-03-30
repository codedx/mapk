# MapK

`MapK` is a utility class that we implemented for Code Dx. It's like a `scala.collection.immutable.Map[K, V]`, but where `K` and `V` are "higher-kinded" types like `Option` or `List`.

It's useful for when you have some "key" class that's parameterized on a type `T`, and you want to store a value of the appropriate type for each key, e.g.

```scala
import com.codedx.util.MapK

abstract class MyKey[T](val name: String)
case object Age extends MyKey[Int]("age")
case object Name extends MyKey[String]("name")
case object NumThings extends MyKey[Int]("numThings")

val info: MapK[MyKey, cats.Id] = /* ... */
val age: Option[Int] = info.get(Age)
val name: Option[String] = info.get(Name)
```

# Developing

I'm in the middle of trying to set this project up to be published via Github Packages.
As such, I'm using the [`sbt-github-packages`](https://github.com/djspiewak/sbt-github-packages) SBT Plugin.
This plugin requires some extra configuration in the form of a `GITHUB_TOKEN` environment variable.
Failing to set this seems to cause SBT/IntelliJ to freak out; IntelliJ won't import the project, and SBT's `update` seems to fail.
As long as you provide a token, everything seems to be ok. 
That said, I'm not sure what would happen if you aren't part of the `codedx` organization.