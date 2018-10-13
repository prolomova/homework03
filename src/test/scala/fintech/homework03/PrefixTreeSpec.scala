package fintech.homework03
import org.scalatest.{FlatSpec, Matchers}

class PrefixTreeSpec extends FlatSpec with Matchers {
  it should "work well with strings" in {
    val tree: IPrefixTree[Char, Int] = new PrefixTree[Char, Int](Map.empty, None)

    val with42: IPrefixTree[Char, Int] = tree.put("abcd", 42)
    with42.sub("ab").sub("cd").get should be (42)

    val withDouble: IPrefixTree[Char, AnyVal] = with42.put("abcde", 13.0)
    withDouble.sub("ab").sub("cd").get should be (42)
    withDouble.sub("ab").sub("cde").get should be (13.0)
  }

  it should "work well with any types" in {
    val tree: IPrefixTree[Any, Any] = new PrefixTree[Any, Any](Map.empty, None)

    val diffTypesTree: IPrefixTree[Any, Any] = tree
      .put("ab", 42)
      .put(Range(0, 2), "Hello")

    diffTypesTree.sub(Range(0, 2)).get should be("Hello")
    diffTypesTree.sub("ab").get should be(42)
  }

  "Not existing key in prefix tree" should "return empty prefix tree" in {
    val emptyTree: IPrefixTree[Any, Any] = new PrefixTree[Any, Any](Map.empty, None)

    val prefixTree: IPrefixTree[Any, Any] = emptyTree.put("ab", 42)

    prefixTree.sub(Range(0, 2)) == emptyTree should be(true)
  }

  "Equal prefix trees" should "be equal" in {
    val tree: IPrefixTree[Char, Int] = new PrefixTree[Char, Int](Map.empty, None)
    val tree1: IPrefixTree[Char, Int] = tree.put("abcd", 42)
    val tree2: IPrefixTree[Char, Int] = tree.put("abcd", 42)
    tree1 == tree2 should be (true)
  }

  "Not equal prefix trees" should "have different hashcodes" in {
    val tree: IPrefixTree[Char, Int] = new PrefixTree[Char, Int](Map.empty, None)
    val tree1: IPrefixTree[Char, Int] = tree.put("abcd", 42)
    val tree2: IPrefixTree[Char, Int] = tree.put("acd", 43)
    tree1.hashCode() != tree2.hashCode() should be (true)
  }

  "Not existing key in prefix tree" should "have no value" in {
    val tree: IPrefixTree[Char, Int] = new PrefixTree[Char, Int](Map.empty, None).put("ab", 1)
    var exception = false
    try tree.sub("a").get
    catch {
      case elementException: NoSuchElementException => exception = true
    }
        exception should be (true)
  }
}

