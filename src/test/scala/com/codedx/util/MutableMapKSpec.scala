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

import cats.arrow.FunctionK
import cats.{ Id, ~> }
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MutableMapKSpec extends AnyFunSpec with Matchers{
	abstract class MyKey[T](val name: String)
	case object Age extends MyKey[Int]("age")
	case object Name extends MyKey[String]("name")
	case object Fingers extends MyKey[Int]("fingers")
	case object Extra extends MyKey[Boolean]("extra")

	def makeDemoMap = {
		new MutableMapK[MyKey, cats.Id]()
			.add(Age, 10)
			.add(Name, "demo")
	}

	describe("MutableMapK") {
		describe(".contains(key)") {
			it ("should return whether or not the map contains the given key") {
				makeDemoMap.contains(Extra) shouldBe false
				makeDemoMap.contains(Age) shouldBe true
			}
		}

		describe(".get(key)") {
			val m = makeDemoMap
			it ("should return Some with an appropriately-typed value if the key is present") {
				val age: Option[Int] = m.get(Age)
				age shouldEqual Some(10)
			}
			it ("should return None if the key is not present") {
				m.get(Extra) shouldEqual None
			}
		}

		describe(".apply(key)") {
			val m = makeDemoMap
			it ("should return an appropriately-typed value if the key is present") {
				(m(Age): Int) shouldEqual 10
			}
			it ("should throw an exception if the key is absent") {
				a[NoSuchElementException] should be thrownBy { m(Extra) }
			}
		}

		describe(".add(key, value)") {
			it ("should mutate the map, adding an entry for the given key and value") {
				val m = makeDemoMap
				val m2 = m.add(Extra, true)
				m eq m2 shouldBe true // reference equality
				m(Extra) shouldEqual true
				m(Age) shouldEqual 10
			}

			it ("should replace existing entries when adding a key that is already present") {
				val m = makeDemoMap
				m.add(Age, 20)
				m(Age) shouldEqual 20
			}
		}

		describe(".remove(key)") {
			it ("should mutate the map, removing the entry for the given key, returning the removed value") {
				val m = makeDemoMap
				val removed = m.remove(Age)
				removed shouldEqual Some(10)
				m.get(Age) shouldEqual None
				m.contains(Name) shouldBe true
			}
			it ("should no-op if the key is already not present") {
				val m = makeDemoMap
				val removed = m.remove(Extra)
				removed shouldEqual None
				m.contains(Extra) shouldBe false
			}
		}

		describe(".getOrElseUpdate(key, =>value)") {
			it ("should get an existing value if the key is present") {
				val m = makeDemoMap
				val name = m.getOrElseUpdate(Name, "derp")
				name shouldEqual "demo"
				m(Name) shouldEqual "demo"
			}
			it ("should update the map with the default value if the key is missing") {
				val m = makeDemoMap
				m.contains(Extra) shouldEqual false
				val b = m.getOrElseUpdate(Extra, true)
				b shouldBe true
				m(Extra) shouldBe true
			}
		}

		describe(".mapValues(f)") {
			it ("should create a new MutableMapK whose values correspond to the original map values, transformed by f") {
				val mapped: MutableMapK[MyKey, List] = makeDemoMap.mapValues(Lambda[cats.Id ~> List](v => List(v, v)))
				mapped(Name) shouldEqual List("demo", "demo")
				mapped(Age) shouldEqual List(10, 10)
			}
		}

		describe(".addFrom(that)") {
			it ("should .add an entry for each entry in 'that' map") {
				val m = makeDemoMap
				val m2 = new MutableMapK[MyKey, cats.Id].add(Extra, true).add(Name, "replaced")

				// preconditions
				m.contains(Extra) shouldBe false
				m(Name) shouldEqual "demo"
				m(Age) shouldBe 10

				m.addFrom(m2)

				// postconditions
				m(Extra) shouldBe true
				m(Name) shouldBe "replaced"
				m(Age) shouldBe 10
			}
		}

		describe(".keys") {
			it ("should return an iterable collection of keys from the map") {
				makeDemoMap.keys.toList should contain theSameElementsAs List(Age, Name)
			}
		}

		describe(".values") {
			it ("should return an iterable collection of values from the map") {
				makeDemoMap.values.toList should contain theSameElementsAs List(10, "demo")
			}
		}

		describe(".foreach(f)") {
			it ("should pass each entry to the callback function, in no particular order") {
				val seenValuesB = List.newBuilder[Any]
				val seenKeysB = Set.newBuilder[MyKey[_]]
				val m = makeDemoMap
				m.foreach(new FunctionK[m.Entry, Lambda[x => Unit]] {
					def apply[A](fa: (MyKey[A], Id[A])) = {
						seenKeysB += fa._1
						seenValuesB += fa._2
					}
				})
				seenKeysB.result() shouldEqual Set(Name, Age)
				seenValuesB.result() should contain theSameElementsAs List(10, "demo")
			}
		}

		describe(".toMap") {
			it ("should convert the map to an immutable MapK") {
				val m: MapK[MyKey, cats.Id] = makeDemoMap.toMap
				m(Name) shouldEqual "demo"
				m(Age) shouldEqual 10
			}
		}

		describe(".isEmpty / .nonEmpty") {
			it ("should indicate whether the map is currently empty") {
				val m = makeDemoMap

				// preconditions
				m.isEmpty shouldBe false
				m.nonEmpty shouldBe true

				// then remove all the entries
				m.remove(Age)
				m.remove(Name)

				// postconditions
				m.isEmpty shouldBe true
				m.nonEmpty shouldBe false
			}
		}
	}
}
