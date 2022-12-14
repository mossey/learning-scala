package patmat


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

/**
 * Assignment 4: Huffman coding
 *
 */
trait Huffman extends HuffmanInterface {

  // Part 1: Basics
  def weight(tree: CodeTree): Int = {
    tree match {
      case leaf: Leaf => leaf.weight
      case fork: Fork => weight(fork.left) + weight(fork.right)
    }
  }

  def chars(tree: CodeTree): List[Char] = {
    tree match {
      case leaf: Leaf => List(leaf.char)
      case fork: Fork => chars(fork.left) ::: chars(fork.right)
    }
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
   * times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   * List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   * val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   * val theChar = pair._1
   * val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   * pair match {
   * case (theChar, theInt) =>
   * println("character is: "+ theChar)
   * println("integer is  : "+ theInt)
   * }
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    def charTime(charsList: List[Char], resList: List[(Char, Int)], char: Char, sum: Int): List[(Char, Int)] = {
      charsList match {
        case Nil => resList :+ (char, sum)
        case _ => charsList.head match {
          case x if x != char => charTime(charsList.tail, resList :+ (char, sum), charsList.head, 1)
          case _ => charTime(charsList.tail, resList, char, sum + 1)
        }
      }

    }

    val list = chars.sortWith((s, t) => s < t)
    if (list.isEmpty)
      Nil
    else
      charTime(list, List(), list.head, 0)
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {

    def insertLeaf(tuples: (Char, Int), resList: List[Leaf]): List[Leaf] = {
      resList match {
        case Nil => List(Leaf(tuples._1, tuples._2))
        case x :: xs => if (tuples._2 < x.weight) Leaf(tuples._1, tuples._2) :: resList else x :: insertLeaf(tuples, xs)
      }
    }

    def makeLeafList(list: List[(Char, Int)], resList: List[Leaf]): List[Leaf] = {
      list match {
        case Nil => resList
        case _ => makeLeafList(list.tail, insertLeaf(list.head, resList))
      }
    }

    makeLeafList(freqs, List())
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = {
    trees match {
      case Nil => false
      case List(x) => true
      case x :: xs => false
    }
  }

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
    trees match {
      case Nil => trees
      case x if x.tail.isEmpty => trees
      case x :: xs => if (xs.tail.isEmpty) trees else combine(makeCodeTree(x, xs.head) :: xs.tail)
    }

  }

  /**
   * This function will be called in the following way:
   *
   * until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   */
  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    trees match {
      case Nil => trees
      case x if x.tail.isEmpty => trees
      case x :: xs => if (done(trees)) until(done, merge)(merge(trees)) else trees
    }

  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {

    def union(list: List[CodeTree]): List[CodeTree] = {
      list match {
        case Nil => list
        case List(x) => list
        case _ =>
          val sortedList = list.sortBy(checkLen).sortBy(checkWeight)
          val firstEl = sortedList.head
          val secondEl = sortedList.tail.head
          val tailList = sortedList.tail.tail
          makeCodeTree(firstEl, secondEl) :: union(tailList)
      }
    }

    def checkLen(codeTree: CodeTree): Int = {
      codeTree match {
        case fork: Fork => fork.chars.length
        case leaf: Leaf => 1
      }
    }

    def checkWeight(codeTree: CodeTree): Int = {
      codeTree match {
        case fork: Fork => fork.weight
        case leaf: Leaf => leaf.weight
      }
    }

    def unionFork(subtree: List[CodeTree]): CodeTree = {
      subtree match {
        case Nil => subtree.head
        case List(x) => x
        case x if x.length == 2 => makeCodeTree(subtree.last, subtree.head)
        case x :: xs =>
          val (first, second) = subtree span (y => checkWeight(y) <= checkWeight(x))
          first.length match {
            case len if len == 1 => unionFork(union(xs.tail ::: union(List(first.head, xs.head))))
            case len if len > 1 => unionFork(union(first) ::: second)
          }
      }
    }

    unionFork(makeOrderedLeafList(times(chars)))

  }

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  /*Decoding also starts at the root of the tree. Given a sequence of bits to decode, we successively
  read the bits, and for each 0, we choose the left branch, and for each 1 we choose the right branch.
  When we reach a leaf, we decode the corresponding character and then start again at the root of the
  tree. As an example, given the Huffman tree above, the sequence of bits,10001010 corresponds to BAC.*/

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {

    def foundChar(bit: List[Bit], subTree: CodeTree): (Char, List[Bit]) = {

      subTree match {
        case fork: Fork => bit match {
          case Nil => throw new NoSuchElementException
          case _ => bit.head match {
            case x if x == 1 => foundChar(bit.tail, fork.right)
            case x if x == 0 => foundChar(bit.tail, fork.left)
          }
        }
        case leaf: Leaf => (leaf.char, bit)
      }
    }

    def bitIter(bitsList: List[Bit], resList: List[Char]): List[Char] = {
      bitsList match {
        case Nil => resList
        case _ => bitIter(foundChar(bitsList, tree)._2, resList :+ foundChar(bitsList, tree)._1)
      }
    }

    bitIter(bits, List())

  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   * http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = {
    decode(frenchCode, secret)
  }
  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  /*Encoding
  For a given Huffman tree, one can obtain the encoded representation of a character by traversing
  from the root of the tree to the leaf containing the character. Along the way, when a left branch
   is chosen, a 0 is added to the representation, and when a right branch is chosen, 1 is added to
   the representation. Thus, for the Huffman tree above, the character D is encoded as 1011.*/

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def createBitList(char: Char, subTree: CodeTree, bitList: List[Bit]): List[Bit] = {
      def check(codeTree: CodeTree): Boolean = {
        codeTree match {
          case fork: Fork => fork.chars.contains(char)
          case leaf: Leaf => leaf.char == char
        }
      }

      subTree match {
        case fork: Fork =>
          if (check(fork.left))
            createBitList(char, fork.left, bitList :+ 0)
          else
            createBitList(char, fork.right, bitList :+ 1)
        case leaf: Leaf => bitList
      }
    }

    def charIter(charList: List[Char], bitList: List[Bit]): List[Bit] = {
      charList match {
        case Nil => bitList
        case _ => charIter(charList.tail, createBitList(charList.head, tree, bitList))
      }
    }

    charIter(text, List())
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table.filter(s => s._1 == char).head._2

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = {

    def converterTree(codeTable: CodeTable, list: List[Char]): CodeTable = {
      list match {
        case Nil => codeTable
        case _ => converterTree(codeTable :+ (list.head, encode(tree)(List(list.head))), list.tail)
      }
    }

    tree match {
      case leaf: Leaf => converterTree(List(), List(leaf.char))
      case fork: Fork => converterTree(List(), fork.chars)
    }
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    def mergerTable(resTable: CodeTable, addedTable: CodeTable): CodeTable = {
      addedTable match {
        case Nil => resTable
        case _ =>
          if (resTable.filter(s => s._1 == addedTable.head._1).isEmpty)
            mergerTable(resTable :+ addedTable.head, addedTable.tail)
          else
            mergerTable(resTable, addedTable.tail)
      }
    }

    mergerTable(a, b)
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codeTable = convert(tree)

    def addedBits(first: List[Bit], second: List[Bit]): List[Bit] = {
      second match {
        case Nil => first
        case _ => addedBits(first :+ second.head, second.tail)
      }
    }

    def iterChar(list: List[Char], resList: List[Bit]): List[Bit] = {
      list match {
        case Nil => resList
        case _ => iterChar(list.tail, addedBits(resList, codeBits(codeTable)(list.head)))
      }
    }

    iterChar(text, List())
  }
}

object Huffman extends Huffman