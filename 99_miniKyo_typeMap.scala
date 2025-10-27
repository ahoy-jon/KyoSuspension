// File: 99_miniKyo_typeMap.scala

package miniKyo

import scala.collection.immutable.TreeSeqMap

/** A type-indexed map that stores values indexed by their type.
  *
  * @param underlying
  *   The underlying TreeSeqMap storage
  * @tparam A
  *   The intersection type of all values in the map
  */
case class TypeMap[+A](underlying: TreeSeqMap[Tag[?], Any] = TreeSeqMap.empty):

  /** Retrieves a value of type B from the TypeMap.
    *
    * @tparam B
    *   The type of the value to retrieve
    * @param t
    *   An implicit Tag for type B
    * @return
    *   The value of type B
    * @throws RuntimeException
    *   if the value is not found
    */
  def get[B](using t: Tag[B]): B =
    underlying
      .getOrElse(
        t,
        throw new RuntimeException(s"TypeMap missing value of type: ${t.tag.repr}")
      )
      .asInstanceOf[B]

  /** Adds a new key-value pair to the TypeMap.
    *
    * @param b
    *   The value to add
    * @tparam B
    *   The type of the value to add
    * @param t
    *   An implicit Tag for type B
    * @return
    *   A new TypeMap with the added key-value pair
    */
  def add[B](b: B)(using t: Tag[B]): TypeMap[A & B] =
    TypeMap(underlying.updated(t, b))

  /** Combines this TypeMap with another TypeMap.
    *
    * @param that
    *   The TypeMap to combine with
    * @tparam B
    *   The type of values in the other TypeMap
    * @return
    *   A new TypeMap containing all key-value pairs from both TypeMaps
    */
  def union[B](that: TypeMap[B]): TypeMap[A & B] =
    TypeMap(underlying ++ that.underlying)

  /** Returns the number of key-value pairs in the TypeMap.
    */
  def size: Int = underlying.size

  /** Checks if the TypeMap is empty.
    */
  def isEmpty: Boolean = underlying.isEmpty

  /** Returns a string representation of the TypeMap.
    */
  def show: String =
    underlying
      .map { case (tag, value) => s"${tag.tag.shortName} -> $value" }
      .mkString("TypeMap(", ", ", ")")

  override def toString: String = show
end TypeMap

object TypeMap:
  /** An empty TypeMap. */
  val empty: TypeMap[Any] = TypeMap()

  /** Creates a TypeMap with a single key-value pair.
    */
  def apply[A](a: A)(using ta: Tag[A]): TypeMap[A] =
    TypeMap(TreeSeqMap(ta -> a))

  /** Creates a TypeMap with two key-value pairs.
    */
  def apply[A, B](a: A, b: B)(using ta: Tag[A], tb: Tag[B]): TypeMap[A & B] =
    TypeMap(TreeSeqMap(ta -> a, tb -> b))

  /** Creates a TypeMap with three key-value pairs.
    */
  def apply[A, B, C](a: A, b: B, c: C)(using ta: Tag[A], tb: Tag[B], tc: Tag[C]): TypeMap[A & B & C] =
    TypeMap(TreeSeqMap(ta -> a, tb -> b, tc -> c))
end TypeMap
