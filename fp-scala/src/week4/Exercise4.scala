package week4

import scala.annotation.tailrec

/**
 * Assignment 4: Huffman coding
 *
 */
object Exercise5 {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree



  // Part 1: Basics

  // def weight(tree: CodeTree): Int = ??? // tree match ...
  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, w) => w
    case Leaf(_, w) => w
  }

  //def chars(tree: CodeTree): List[Char] = ??? // tree match ...
  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, c, _) => c
    case Leaf(c, _) => List(c)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   *   
   *   AUTHOR's NOTE: I'm not preserving the order of occurance.
   *   The routines are implemented tail recursive whenever possible.
   */
  def times(chars: List[Char]): List[(Char, Int)] = chars match {
    case x :: xs => {
      @tailrec
      def timesIter(tuples: List[(Char, Int)], y: Char, ys: List[Char]): List[(Char, Int)] = {
        /* Check if y is found in tups */
        def isPresent(tups: List[(Char, Int)]): Boolean = {
          @tailrec
          def isPresentIter(tups: List[(Char, Int)]): Boolean = {
            if (tups.isEmpty) false
            else if (tups.head._1 == y) true
            else isPresentIter(tups.tail)
          }
          isPresentIter(tups)
        }
        /* If y is found in tups, increment it. @Pre: y SHOULD be in tups */
        def increment(tups: List[(Char, Int)]): List[(Char, Int)] = {
          @tailrec
          def incrementIter(acc:List[(Char, Int)], t: List[(Char, Int)]): List[(Char, Int)] = {
            if(t.isEmpty) acc
            else incrementIter((t.head._1, t.head._2 + { if (t.head._1 == y) 1 else 0 }) :: acc, t.tail)
          }
          incrementIter(List(), tups)
        }
        /* Actual routine start */
        val newTuple = if (isPresent(tuples)) increment(tuples) else (y, 1) :: tuples
        if(ys.isEmpty) newTuple
        else timesIter(newTuple, ys.head, ys.tail)
      }
      timesIter(Nil, x, xs)
    }
    case Nil => List()
  }
  /* A short and sweet implementation of the above */
  def times2(chars: List[Char]): List[(Char, Int)] = chars.groupBy(identity).mapValues(x => x.size).toList

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def transformIntoLeaf(freqs: List[(Char, Int)]): List[Leaf] = {
      freqs.map(x => Leaf(x._1, x._2))
    }
    def insertionSort(freqs: List[(Char, Int)]): List[(Char, Int)] = freqs match {
      case x :: xs => {
        def insert(y: (Char, Int), ys: List[(Char, Int)]): List[(Char, Int)] = ys match {
          case z :: zs => if (y._2 > z._2) z :: insert(y, zs) else y :: z :: zs
          case Nil => List(y)
        }
        insert(x, insertionSort(xs))
      }
      case Nil => List()
    }
    transformIntoLeaf(insertionSort(freqs))
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    def insert(y: CodeTree, ys: List[CodeTree]): List[CodeTree] = ys match {
      case z :: zs => if (weight(y) > weight(z)) z :: insert(y, zs) else y :: z :: zs
      case Nil => List(y)
    }
    if (trees.length < 2) trees
    else insert(makeCodeTree(trees.head, trees.tail.head), trees.tail.tail)
  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def until(stopCondition: List[CodeTree] => Boolean, applyFunction: List[CodeTree] => List[CodeTree])(target: List[CodeTree]): List[CodeTree] = {
    if(stopCondition(target)) target
    else until(stopCondition, applyFunction)(applyFunction(target))
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars))).head


  /* Reverse a list of characters */
  private def reverse[T](list: List[T]): List[T] = {
    def revIter(acc: List[T], rem: List[T]): List[T] = {
      if(rem.isEmpty) acc else revIter(rem.head :: acc, rem.tail)
    }
    revIter(List(), list)
  }

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    /* Find a character, return (<found char>, remaining bit sequence) */
    def findCharacter(t: CodeTree, bs: List[Bit]): (Char, List[Bit]) = t match {
      case Fork(l, r, _, _) => bs match {
        case 0 :: xs => findCharacter(l, xs)
        case 1 :: xs => findCharacter(r, xs)
        case _ => throw new IllegalArgumentException("Invalid bit sequence")
      }
      case Leaf(c, _) => (c, bs)
    }
    /* Keeps finding characters one after another using remaining bit sequence */
    /* def decodeInner2(reverseAcc: List[Char], t: CodeTree, b: List[Bit]): List[Char] = findCharacter(t, b) match {
      case (c, Nil) => reverse(c :: reverseAcc)
      case (c, bs) => decodeInner2(c :: reverseAcc, t, bs)
    } */
    def decodeInner(reverseAcc: List[Char], t: CodeTree, bs: List[Bit]): List[Char] = {
      if (bs.isEmpty) reverse(reverseAcc)
      else {
        val next = findCharacter(t, bs)
        decodeInner(next._1 :: reverseAcc, t, next._2)
      }
    }
    decodeInner(List(), tree, bits)
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)



  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] =  {
    def encodeChar(t: CodeTree, c: Char): List[Bit] = t match {
      case Fork(l, r, _, _) => {
        if(chars(l).contains(c)) 0 :: encodeChar(l, c)
        else 1 :: encodeChar(r, c)
      }
      case Leaf(_, _) => List()
    }
    if(text.isEmpty) List() else encodeChar(tree, text.head) ::: encode(tree)(text.tail)
  }


  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
     def search(t: CodeTable): List[Bit] = {
       if(t.isEmpty) throw new NoSuchElementException("Character not found")
       else if(t.head._1 == char) t.head._2
       else search(t.tail)
     }
     search(table)
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = {
    
    def convertIter(bs: List[Bit], t: CodeTree): CodeTable = t match {
      case Fork(l, r, _, _) => mergeCodeTables(convertIter(0 :: bs, l), convertIter(1 :: bs, r))
      case Leaf(c, _) => List((c, reverse(bs)))
    }
    convertIter(List(), tree)
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    if (a.isEmpty) b
    else mergeCodeTables(a.tail, a.head :: b)
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    // Generate table only once
    val table = convert(tree)
    def quickEncodeIter(txt: List[Char]): List[Bit] = {
      if(txt.isEmpty) List() else codeBits(table)(txt.head) ::: quickEncodeIter(txt.tail)
    }
    quickEncodeIter(text)
  }

}
