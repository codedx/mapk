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

import scala.collection.mutable.{ Map => MutableMap }
import scala.language.existentials
import cats.arrow.FunctionK
import cats.~>

/** A map where Keys have type parameters, and values stored for each key have a related type.
  * I.e. looking up a key of type `K[T]` will result in a value of type `V[T]`.
  * A simple map will use `V = Id`, so that `get(K[T])` results in a bare `T`.
  *
  * @param inner
  * @tparam K
  * @tparam V
  */
class MutableMapK[K[*], V[*]] private(private val inner: MutableMap[K[Any], V[Any]]) { self =>
	def this() = this(MutableMap.empty)

	override def hashCode(): Int = inner.hashCode()
	override def equals(obj: Any): Boolean = obj match {
		case m: MutableMapK[_, _] => m.inner == inner
		case _ => false
	}

	type Entry[x] = (K[x], V[x])
	def entry[T](key: K[T], value: V[T]): Entry[T] = (key, value)

	override def toString = inner.toString

	def contains[T](key: K[T]): Boolean = inner.contains(key.asInstanceOf[K[Any]])
	def get[T](key: K[T]): Option[V[T]] = inner.get(key.asInstanceOf[K[Any]]).map(_.asInstanceOf[V[T]])
	def apply[T](key: K[T]): V[T] = inner(key.asInstanceOf[K[Any]]).asInstanceOf[V[T]]
	def add[T](key: K[T], value: V[T]): this.type = {
		inner.put(key.asInstanceOf[K[Any]], value.asInstanceOf[V[Any]])
		this
	}
	def add[T](entry: Entry[T]): this.type = add(entry._1, entry._2)
	def remove[T](key: K[T]): Option[V[T]] = {
		inner.remove(key.asInstanceOf[K[Any]]).map(_.asInstanceOf[V[T]])
	}
	def getOrElseUpdate[T](key: K[T], value: => V[T]) = get(key) match {
		case None =>
			val v = value
			add(key, v)
			v
		case Some(v) =>
			v
	}
	def mapValues[U[_]](f: V ~> U) = {
		val mapped = new MutableMapK[K, U]
		this.foreach(new FunctionK[Entry, ({ type U[x] = Unit })#U] {
			def apply[A](kv: (K[A], V[A])): Unit = mapped.add(kv._1, f(kv._2))
		})
		mapped
	}
	def addFrom(that: MutableMapK[K, V]): this.type = {
		that.foreach(new FunctionK[Entry, ({ type U[x] = Unit })#U] {
			def apply[A](kv: (K[A], V[A])): Unit = self.add(kv._1, kv._2)
		})
		this
	}
	def keys = inner.keySet
	def foreach(f: Entry ~> ({ type U[x] = Unit })#U) = {
		def handleKv[A](kv: (K[Any], V[Any])) = f(MapK.tuple(kv._1.asInstanceOf[K[A]], kv._2.asInstanceOf[K[A]]))
		for (kv <- inner) handleKv(kv)
	}

	def toMap: MapK[K, V] = MapK.coerce[K, V](inner.toMap)

	def isEmpty = inner.isEmpty
	def nonEmpty: Boolean = !isEmpty
	def values = inner.values
}
