package miniKyo

import scala.collection.immutable.TreeSeqMap
import scala.reflect.ClassTag

/** A runtime representation of a type, used for type-level operations.
  */
trait Tag[T]:
    def erased: Tag[Any]
    def show: String
    def <:<[U](that: Tag[U]): Boolean
    def =:=[U](that: Tag[U]): Boolean
end Tag

object Tag:
    private class TagImpl[T](using ct: ClassTag[T]) extends Tag[T]:
        override def equals(other: Any): Boolean =
            other match
                case that: TagImpl[?] =>
                    (this eq that) || (this.classTag.runtimeClass == that.classTag.runtimeClass)
                case _ => false

        override def hashCode(): Int = ct.runtimeClass.hashCode

        def classTag: ClassTag[T] = ct

        override def toString: String = s"Tag(${ct.runtimeClass.getSimpleName})"

        def erased: Tag[Any] = this.asInstanceOf[Tag[Any]]
        
        def show: String = ct.runtimeClass.getSimpleName
        
        def <:<[U](that: Tag[U]): Boolean =
            that match
                case thatImpl: TagImpl[?] =>
                    thatImpl.classTag.runtimeClass.isAssignableFrom(ct.runtimeClass)
                case _ => false
        
        def =:=[U](that: Tag[U]): Boolean = this.equals(that)
    end TagImpl

    def apply[T](using ct: ClassTag[T]): Tag[T] = new TagImpl[T]
    
    // Implicit conversion from ClassTag to Tag
    given tagFromClassTag[T](using ct: ClassTag[T]): Tag[T] = new TagImpl[T]
end Tag

/** A type-indexed map that stores values indexed by their type.
  *
  * @param underlying
  *   The underlying TreeSeqMap storage
  * @tparam A
  *   The intersection type of all values in the map
  */
case class TypeMap[+A](underlying: TreeSeqMap[Tag[Any], Any] = TreeSeqMap.empty):
    
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
        underlying.getOrElse(
            t.erased,
            throw new RuntimeException(s"TypeMap missing value of type: ${t.show}")
        ).asInstanceOf[B]

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
        TypeMap(underlying.updated(t.erased, b))

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
            .map { case (tag, value) => s"${tag.show} -> $value" }
            .mkString("TypeMap(", ", ", ")")

    override def toString: String = show
end TypeMap

object TypeMap:
    /** An empty TypeMap. */
    val empty: TypeMap[Any] = TypeMap()

    /** Creates a TypeMap with a single key-value pair.
      */
    def apply[A](a: A)(using ta: Tag[A]): TypeMap[A] =
        TypeMap(TreeSeqMap(ta.erased -> a))

    /** Creates a TypeMap with two key-value pairs.
      */
    def apply[A, B](a: A, b: B)(using ta: Tag[A], tb: Tag[B]): TypeMap[A & B] =
        TypeMap(TreeSeqMap(ta.erased -> a, tb.erased -> b))

    /** Creates a TypeMap with three key-value pairs.
      */
    def apply[A, B, C](a: A, b: B, c: C)(using ta: Tag[A], tb: Tag[B], tc: Tag[C]): TypeMap[A & B & C] =
        TypeMap(TreeSeqMap(ta.erased -> a, tb.erased -> b, tc.erased -> c))
end TypeMap

