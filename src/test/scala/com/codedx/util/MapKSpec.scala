/* Copyright 2021 Code Dx, Inc
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.codedx.util

import scala.collection.mutable

import cats.{ Id, ~> }
import cats.arrow.FunctionK
import cats.data.Tuple2K
import cats.kernel.Monoid
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MapKSpec extends AnyFunSpec with Matchers {
	abstract class MyKey[T](val name: String)
	case object Age extends MyKey[Int]("age")
	case object Name extends MyKey[String]("name")
	case object Fingers extends MyKey[Int]("fingers")
	case object Extra extends MyKey[Boolean]("extra")

	val ezio = MapK(
		Tuple2K[MyKey, cats.Id, Int](Age, 65),
		Tuple2K[MyKey, cats.Id, String](Name, "Ezio Auditore"),
		Tuple2K[MyKey, cats.Id, Int](Fingers, 9)
	)
	val dylan = MapK.empty[MyKey, cats.Id]
		.updated(Age, 32)
		.updated(Name, "Dylan")
		.updated(Fingers, 10)

	val multi = MapK.empty[MyKey, List]
		.updated(Age, List(1,2,3,4))
		.updated(Name, List("a", "b"))

	describe("MapK") {
		describe("entrySyntax") {
			import MapK.entrySyntax._
			it ("should allow for a convenient MapK.apply with cats.Id value types") {
				val appliedMap = MapK(Age ~>> 32, Name ~>> "Dylan", Fingers ~>> 10)
				appliedMap shouldEqual dylan
			}
			it ("should allow for a convenient MapK.apply with higher-kinded value types") {
				val appliedMap = MapK(Age ~> List(1, 2, 3, 4), Name ~> List("a", "b"))
				appliedMap shouldEqual multi
			}
		}
		
		describe("equals and hashCode") {
			import MapK.entrySyntax._
			it ("should behave properly for identical MapK instances") {
				val a = MapK(Age ~>> 21, Name ~>> "John Doe", Fingers ~>> 10)
				val b = MapK(Age ~>> 21, Name ~>> "John Doe", Fingers ~>> 10)
				a.equals(b) shouldBe true
				a.hashCode shouldEqual b.hashCode
			}
			it ("should behave properly for non-identical MapK instances") {
				val a = MapK(Age ~>> 21, Name ~>> "John Doe")
				val b = MapK(Age ~>> 21, Name ~>> "John Doe", Fingers ~>> 10)
				a.equals(b) shouldBe false
				a.hashCode should not equal b.hashCode
			}
		}

		describe(".untyped") {
			it("should return a regular Map containing the same entries, minus the explicit type information") {
				val u = ezio.untyped
				u shouldEqual Map[MyKey[_], Any](Age -> 65, Name -> "Ezio Auditore", Fingers -> 9)
			}
		}

		describe(".contains(key)") {
			it ("should return false when the map does not contain the given key") {
				ezio.contains(Extra) shouldBe false
			}
			it ("should return true when the map contains the given key") {
				ezio.contains(Age) shouldBe true
			}
		}

		describe(".get(key)") {
			it ("should return Some with an appropriately-typed value if the key is present") {
				val age: Option[Int] = ezio.get(Age)
				age shouldEqual Some(65)

				(multi.get(Age): Option[List[Int]]) shouldEqual Some(List(1,2,3,4))
			}
			it ("should return None if the key is not present") {
				ezio.get(Extra) shouldEqual None
			}
		}

		describe(".apply(key)") {
			it ("should return an appropriately-typed value if the key is present") {
				(ezio(Age): Int) shouldEqual 65
				(dylan(Age): Int) shouldEqual 32
			}
			it ("should throw an exception if the key is absent") {
				a[NoSuchElementException] should be thrownBy { ezio(Extra) }
				a[NoSuchElementException] should be thrownBy { multi(Fingers) }
			}
		}

		describe(".updated(key, value)") {
			it ("should create a new MapK which includes the key and value") {
				val ezio2 = ezio.updated(Extra, true)
				ezio.contains(Extra) shouldBe false
				ezio2.contains(Extra) shouldBe true
				ezio2(Extra) shouldBe true
			}
			it ("should replace the value for the given key if an entry with that key already existed") {
				val ezio2 = ezio.updated(Fingers, 8)
				ezio(Fingers) shouldEqual 9
				ezio2(Fingers) shouldEqual 8
			}
		}

		describe("+ (key, value)") {
			it ("should create a new MapK which includes the key and value") {
				val ezio2 = ezio + (Extra, true)
				ezio.contains(Extra) shouldBe false
				ezio2.contains(Extra) shouldBe true
				ezio2(Extra) shouldBe true
			}
			it ("should replace the value for the given key if an entry with that key already existed") {
				val ezio2 = ezio + (Fingers, 8)
				ezio(Fingers) shouldEqual 9
				ezio2(Fingers) shouldEqual 8
			}
		}

		describe("++ (that)") {
			val map1 = MapK.empty[MyKey, cats.Id]
				.updated(Fingers, 10)
				.updated(Name, "Number 1")
			val map2 = MapK.empty[MyKey, cats.Id]
				.updated(Extra, false)
				.updated(Name, "Number 2")

			it ("should create a new MapK which includes values from both, with priority to the map on the RHS of the operator") {
				val map12 = map1 ++ map2
				val map21 = map2 ++ map1

				map1.contains(Extra) shouldBe false
				map12(Extra) shouldBe false
				map21(Extra) shouldBe false
				map12(Name) shouldBe "Number 2"
				map21(Name) shouldBe "Number 1"
				map12(Fingers) shouldBe 10
				map21(Fingers) shouldBe 10
			}
		}

		describe(".foreach(f)") {
			it ("should pass each entry to the callback function, in no particular order") {
				val seenValuesB = List.newBuilder[Any]
				val seenKeysB = Set.newBuilder[MyKey[_]]
				ezio.foreach(new FunctionK[ezio.Entry, ({ type F[x] = Unit })#F] {
					def apply[A](fa: (MyKey[A], Id[A])) = {
						seenKeysB += fa._1
						seenValuesB += fa._2
					}
				})
				seenKeysB.result() shouldEqual Set(Name, Age, Fingers)
				seenValuesB.result() should contain theSameElementsAs List(9, 65, "Ezio Auditore")
			}
		}

		describe(".mapValues(f)") {
			it ("should create a new MapK whose values correspond to the original map values, transformed by f") {
				val ezioList = ezio.mapValues(new FunctionK[cats.Id, List] {
					def apply[A](a: A) = List(a, a)
				})
				ezioList(Name) shouldEqual List("Ezio Auditore", "Ezio Auditore")
				ezioList(Age) shouldEqual List(65, 65)
				ezioList(Fingers) shouldEqual List(9, 9)
			}
		}

		describe(".map(f)") {
			it ("should create a new MapK whose entries correspond to the original map entries, transformed by f") {
				val restoreFinger = new FunctionK[ezio.Entry, cats.Id] {
					def apply[A](entry: ezio.Entry[A]) = entry match {
						case (Fingers, n) => n + 1
						case (k, v) => v
					}
				}
				val ezio2 = ezio.map(restoreFinger)
				ezio(Fingers) shouldEqual 9
				ezio2(Fingers) shouldEqual 10
				ezio2(Age) shouldEqual 65
				ezio2(Name) shouldEqual "Ezio Auditore"
			}
		}

		describe(".keys") {
			it ("should return an iterable collection of the map's keys") {
				ezio.keys.toList should contain theSameElementsAs List(Age, Name, Fingers)
				multi.keys.toList should contain theSameElementsAs List(Age, Name)
			}
		}

		describe(".values") {
			it ("should return an interable collection of the map's values") {
				ezio.values.toList should contain theSameElementsAs List("Ezio Auditore", 9, 65)
			}
		}

		describe(".merge") {
			it ("should create a new MapK which combines values from both operands via the combiner function") {
				val max = new FunctionK[MyKey, MapK[MyKey, cats.Id]#Combiner] {
					def apply[A](fa: MyKey[A]) = fa match {
						case Name => (l: String, r: String) => (if (l.compareToIgnoreCase(r) > 0) l else r): String
						case Age => (l: Int, r: Int) => l max r
						case Fingers => (l: Int, r: Int) => l max r
					}
				}
				val merged1 = ezio.merge(dylan, max)
				val merged2 = dylan.merge(ezio, max)

				merged1.values.toList should contain theSameElementsAs merged2.values.toList
				merged1(Name) shouldEqual "Ezio Auditore"
				merged1(Age) shouldEqual 65
				merged1(Fingers) shouldEqual 10
			}
		}

		describe(".isEmpty / .nonEmpty") {
			val emptyMap = MapK.empty[MyKey, List]

			it ("should indicate whether the map is empty or not") {
				emptyMap.isEmpty shouldBe true
				ezio.isEmpty shouldBe false

				emptyMap.nonEmpty shouldBe false
				ezio.nonEmpty shouldBe true
			}
		}

		describe("Monoid instance") {
			import cats.instances.list._
			val M = Monoid[MapK[MyKey, List]]

			it("should be able to create an empty MapK") {
				M.empty.isEmpty shouldBe true
				M.empty.keys.isEmpty shouldBe true
				M.empty.values.isEmpty shouldBe true
			}

			it ("should use the value's SemigroupK to combine values") {
				val l = M.empty
					.updated(Name, List("left"))
					.updated(Age, List(10))
				val r = M.empty
					.updated(Name, List("right"))

				val lr = M.combine(l, r)
				val rl = M.combine(r, l)

				lr(Name) shouldEqual List("left", "right")
				rl(Name) shouldEqual List("right", "left")
				lr(Age) shouldEqual List(10)
				rl(Age) shouldEqual List(10)
			}
		}
	}
}
