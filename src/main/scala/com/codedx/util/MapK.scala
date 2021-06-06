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

import scala.language.existentials

import cats.arrow.FunctionK
import cats.data.Tuple2K
import cats.{ Monoid, SemigroupK, ~> }

class MapK[K[*], V[*]] private[MapK](val untyped: Map[K[Any], V[Any]]) { self =>
	type Entry[x] = (K[x], V[x])
	type Combiner[x] = (V[x], V[x]) => V[x]

	override def equals(obj: Any): Boolean = obj match {
		case that: MapK[?, ?] => this.untyped == that.untyped
		case _ => false
	}
	override def hashCode(): Int = untyped.hashCode()

	@inline private def asAny[X[_], A](x: X[A]): X[Any] = x.asInstanceOf[X[Any]]

	def contains[A](key: K[A]): Boolean = untyped.contains(asAny(key))
	def get[T](key: K[T]): Option[V[T]] = untyped.get(asAny(key)).map(_.asInstanceOf[V[T]])
	def apply[T](key: K[T]): V[T] = untyped(asAny(key)).asInstanceOf[V[T]]
	def +[T](key: K[T], value: V[T]): MapK[K, V] = new MapK[K, V](untyped + (asAny(key) -> asAny(value)))
	def updated[T](key: K[T], value: V[T]): MapK[K, V] = this.+(key, value)
	def ++(that: MapK[K, V]): MapK[K, V] = {
		val out = Map.newBuilder[K[Any], V[Any]]
		val add = new FunctionK[Entry, ({ type U[x] = Unit })#U] {
			def apply[A](fa: (K[A], V[A])): Unit = out += (asAny(fa._1) -> asAny(fa._2))
		}
		this.foreach(add)
		that.foreach(add)
		new MapK[K, V](out.result())
	}
	def foreach(f: Entry ~> ({ type U[x] = Unit })#U): Unit = {
		def handleKv[A](kv: (K[Any], V[Any])) = f(MapK.tuple(kv._1.asInstanceOf[K[A]], kv._2.asInstanceOf[V[A]]))
		for (kv <- untyped) handleKv(kv)
	}
	def mapValues[V2[_]](f: V ~> V2): MapK[K, V2] = {
		val mapped = Map.newBuilder[K[Any], V2[Any]]
		def handleKv[A](kv: (K[Any], V[Any])): Unit = mapped += (kv._1.asInstanceOf[K[Any]] -> asAny(f(kv._2.asInstanceOf[V[A]])))
		for(kv <- untyped) handleKv(kv)
		MapK.coerce[K, V2](mapped.result())
	}
	def map[V2[_]](f: Entry ~> V2): MapK[K, V2] = {
		val mapped = Map.newBuilder[K[Any], V2[Any]]
		def handleKv[A](kv: (K[Any], V[Any])) = mapped += (kv._1.asInstanceOf[K[Any]] -> f(MapK.tuple[K, V, A](kv._1.asInstanceOf[K[A]], kv._2.asInstanceOf[V[A]])).asInstanceOf[V2[Any]])
		for(kv <- untyped) handleKv(kv)
		MapK.coerce[K, V2](mapped.result())
	}
	def keys: Iterable[K[Any]] = untyped.keys

	def merge(that: MapK[K, V], combiner: K ~> Combiner): MapK[K, V] = {
		var out: MapK[K, V] = self
		that.foreach(new FunctionK[that.Entry, ({ type U[x] = Unit })#U] {
			def apply[A](kv: (K[A], V[A])): Unit = {
				val (k, newValue) = kv
				val v2 = self.get(k) match {
					case Some(oldValue) => combiner(k)(oldValue, newValue)
					case None => newValue
				}
				out = out + (k, v2)
			}
		})
		out
	}

	def isEmpty = untyped.isEmpty
	def nonEmpty: Boolean = !isEmpty
	def values = untyped.values

	override def toString() = {
		val sb = new StringBuilder()
		sb append "TypedMap {\n"
		foreach(new FunctionK[Entry, ({ type U[x] = Unit })#U] {
			def apply[A](entry: Entry[A]): Unit = {
				sb append "  " append entry._1.toString append ": " append entry._2.toString append "\n"
			}
		})
		sb append "}"
		sb.toString
	}
}

object MapK {

	private[util] def tuple[K[_], V[_], T](key: K[T], value: Any): (K[T], V[T]) = (key, value.asInstanceOf[V[T]])

	def coerce[K[_], V[_]](map: Map[K[Any], V[Any]]): MapK[K, V] = new MapK[K, V](map)

	def empty[K[_], V[_]]: MapK[K, V] = new MapK[K, V](Map.empty)

	def apply[K[_], V[_]](entries: Tuple2K[K, V, ?]*): MapK[K, V] = coerce[K, V] {
		val mb = Map.newBuilder[K[Any], V[Any]]
		for (t <- entries) mb += (t.first.asInstanceOf[K[Any]] -> t.second.asInstanceOf[V[Any]])
		mb.result()
	}

	implicit def catsMonoidForMapK[K[_], V[_]](implicit V: SemigroupK[V]): Monoid[MapK[K, V]] = new Monoid[MapK[K, V]] {
		def empty: MapK[K, V] = MapK.empty[K, V]
		def combine(x: MapK[K, V], y: MapK[K, V]): MapK[K, V] = {
			val getCombiner = new FunctionK[K, MapK[K, V]#Combiner] {
				def apply[A](fa: K[A]) = V.algebra[A].combine _
			}
			x.merge(y, getCombiner)
		}
	}

	/** Convenience syntax for constructing `Tuple2K` instances to use with `MapK.apply`
	  */
	object entrySyntax {
		implicit class RichKey[K[_], A](key: K[A]) {
			def ~>[V[_]](value: V[A]) = Tuple2K[K, V, A](key, value)
			def ~>>(value: A) = Tuple2K[K, cats.Id, A](key, value)
		}
	}
}
