package miniKyo

import munit.FunSuite
import scala.reflect.ClassTag

class TypeMapTests extends FunSuite:
    
    // Global given instances for common types
    implicit val stringTag: ClassTag[String] = ClassTag(classOf[String])
    implicit val intTag: ClassTag[Int] = ClassTag(classOf[Int])
    implicit val doubleTag: ClassTag[Double] = ClassTag(classOf[Double])
    implicit val booleanTag: ClassTag[Boolean] = ClassTag(classOf[Boolean])
    
    test("empty TypeMap should be empty"):
        val tm = TypeMap.empty
        assert(tm.isEmpty)
        assertEquals(tm.size, 0, "Size should be 0")
    
    test("create TypeMap with single value"):
        val tm = TypeMap("hello")
        assert(!tm.isEmpty, "TypeMap should not be empty")
        assertEquals(tm.size, 1, "Size should be 1")
        assertEquals(tm.get[String], "hello", "Should retrieve the string value")
    
    test("create TypeMap with two values"):
        val tm = TypeMap("hello", 42)
        assertEquals(tm.size, 2, "Size should be 2")
        assertEquals(tm.get[String], "hello", "Should retrieve the string value")
        assertEquals(tm.get[Int], 42, "Should retrieve the int value")
    
    test("create TypeMap with three values"):
        val tm = TypeMap("hello", 42, 3.14)
        assertEquals(tm.size, 3, "Size should be 3")
        assertEquals(tm.get[String], "hello", "Should retrieve the string value")
        assertEquals(tm.get[Int], 42, "Should retrieve the int value")
        assertEquals(tm.get[Double], 3.14, "Should retrieve the double value")
    
    test("add values to TypeMap"):
        val tm1 = TypeMap.empty
        val tm2 = tm1.add("hello")
        val tm3 = tm2.add(42)
        val tm4 = tm3.add(true)
        
        assertEquals(tm4.size, 3, "Size should be 3")
        assertEquals(tm4.get[String], "hello", "Should retrieve the string value")
        assertEquals(tm4.get[Int], 42, "Should retrieve the int value")
        assertEquals(tm4.get[Boolean], true, "Should retrieve the boolean value")
    
    test("adding values should not mutate original TypeMap"):
        val tm1 = TypeMap("hello")
        val tm2 = tm1.add(42)
        
        assertEquals(tm1.size, 1, "Original TypeMap size should be 1")
        assertEquals(tm2.size, 2, "New TypeMap size should be 2")
    
    test("union two TypeMaps"):
        val tm1 = TypeMap("hello", 42)
        val tm2 = TypeMap(3.14, true)
        val tm3 = tm1.union(tm2)
        
        assertEquals(tm3.size, 4, "Union should have 4 elements")
        assertEquals(tm3.get[String], "hello", "Should retrieve the string value")
        assertEquals(tm3.get[Int], 42, "Should retrieve the int value")
        assertEquals(tm3.get[Double], 3.14, "Should retrieve the double value")
        assertEquals(tm3.get[Boolean], true, "Should retrieve the boolean value")
    
    test("union with overlapping types should overwrite"):
        val tm1 = TypeMap("first")
        val tm2 = TypeMap("second")
        val tm3 = tm1.union(tm2)
        
        assertEquals(tm3.size, 1, "Should have 1 element after union")
        assertEquals(tm3.get[String], "second", "Should have the second value")
    
    test("get on missing type should throw exception"):
        implicit val floatTag: ClassTag[Float] = ClassTag(classOf[Float])
        
        val tm: TypeMap[Any] = TypeMap("hello", 42)
        intercept[RuntimeException]:
            tm.get[Float]  // Float is not in the map
    
    test("TypeMap show should display contents"):
        val tm = TypeMap("hello", 42)
        val shown = tm.show
        
        assert(shown.contains("String"), "Show should contain String")
        // Int is shown as "Integer" in Java, but we need to check what it actually shows
        assert(shown.contains("hello"), "Show should contain hello")
        assert(shown.contains("42"), "Show should contain 42")
    
    test("TypeMap with custom case classes"):
        case class Person(name: String, age: Int)
        case class Address(street: String, city: String)
        
        implicit val personTag: ClassTag[Person] = ClassTag(classOf[Person])
        implicit val addressTag: ClassTag[Address] = ClassTag(classOf[Address])
        
        val person = Person("Alice", 30)
        val address = Address("123 Main St", "Springfield")
        val tm = TypeMap(person, address)
        
        assertEquals(tm.get[Person], person, "Should retrieve the person")
        assertEquals(tm.get[Address], address, "Should retrieve the address")
    
    test("TypeMap preserves value types"):
        implicit val listIntTag: ClassTag[List[Int]] = ClassTag(classOf[List[Int]])
        implicit val setStringTag: ClassTag[Set[String]] = ClassTag(classOf[Set[String]])
        
        val tm = TypeMap(List(1, 2, 3), Set("a", "b"))
        
        assertEquals(tm.get[List[Int]], List(1, 2, 3), "Should retrieve the list")
        assertEquals(tm.get[Set[String]], Set("a", "b"), "Should retrieve the set")
    
    test("can update existing type in TypeMap"):
        val tm1 = TypeMap("first", 1)
        val tm2 = tm1.add("second")
        
        assertEquals(tm2.get[String], "second", "Should retrieve the updated string")
        assertEquals(tm2.get[Int], 1, "Should still retrieve the int")
        assertEquals(tm2.size, 2, "Size should still be 2")
    
    test("empty TypeMap show"):
        val tm = TypeMap.empty
        assertEquals(tm.show, "TypeMap()", "Empty TypeMap should show correctly")

end TypeMapTests

class TagTests extends FunSuite:
    
    implicit val stringTag: ClassTag[String] = ClassTag(classOf[String])
    implicit val intTag: ClassTag[Int] = ClassTag(classOf[Int])
    
    test("Tag equality for same type"):
        val tag1 = Tag[String]
        val tag2 = Tag[String]
        assert(tag1 == tag2, "Same type tags should be equal")
        assert(tag1 =:= tag2, "Same type tags should be equal with =:=")
    
    test("Tag inequality for different types"):
        val tag1 = Tag[String]
        val tag2 = Tag[Int]
        assert(tag1 != tag2, "Different type tags should not be equal")
        assert(!(tag1 =:= tag2), "Different type tags should not be equal with =:=")
    
    test("Tag show returns type name"):
        val tag = Tag[String]
        assertEquals(tag.show, "String", "Tag show should return the type name")
    
    test("Tag hashCode is consistent"):
        val tag1 = Tag[Int]
        val tag2 = Tag[Int]
        assertEquals(tag1.hashCode, tag2.hashCode, "Hash codes should be consistent for same types")
    
    test("Tag subtype checking"):
        trait Animal
        class Dog extends Animal
        
        implicit val animalClassTag: ClassTag[Animal] = ClassTag(classOf[Animal])
        implicit val dogClassTag: ClassTag[Dog] = ClassTag(classOf[Dog])
        
        val animalTag = Tag[Animal]
        val dogTag = Tag[Dog]
        
        // Dog <:< Animal should be true
        assert(dogTag <:< animalTag, "Dog should be subtype of Animal")
        
        // Animal <:< Dog should be false
        assert(!(animalTag <:< dogTag), "Animal should not be subtype of Dog")
    
    test("Tag can be used as map key"):
        val map = Map(Tag[String] -> "hello", Tag[Int] -> 42)
        
        assertEquals(map.get(Tag[String]), Some("hello"), "Should retrieve value by String tag")
        assertEquals(map.get(Tag[Int]), Some(42), "Should retrieve value by Int tag")

end TagTests

