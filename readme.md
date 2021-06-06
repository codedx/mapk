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

// alternate apply syntax:
import MapK.entrySyntax._
val info = MapK(Age ~>> 21, Name ~>> "Dylan")

val age: Option[Int] = info.get(Age) // Some(21)
val name: Option[String] = info.get(Name) // Some("Dylan")
val numThings: Option[Int] = info.get(NumThings) // None
```

# Install

In SBT:

```scala
libraryDependencies += "com.codedx" %% "mapk" % "1.1.0"
```

Disregard the "packages" on the sidebar. Despite this being a public project, you still need to provide a personal access token to download the packages. Very annoying.
