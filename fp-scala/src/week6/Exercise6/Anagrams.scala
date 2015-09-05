package week6.Exercise6

/**
 * Exercise 6: Anagrams
 * @author thirumal
 */
object Anagrams {
  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lower case.
   *
   *  Any list of pairs of lower case characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** Private routine that loads the dictionary into memory */
  private def loadDictionary = {
    val dictionaryPath = "src/week6/Exercise6/linuxwords.txt"
    val wordstream = Option {
      new java.io.FileInputStream(dictionaryPath)
    } getOrElse {
      throw new java.io.FileNotFoundException("Dictionary file " + dictionaryPath + " not found")
    }
    try {
      val s = io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary // Customized for my needs

  /**
   * Converts the word into its character occurrence list.
   *
   *  Note: the upper case and lower case version of the character are treated as the
   *  same character, and are represented as a lower case character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    w.groupBy(_.toLower).toList.map { case (chars, listOfChars) => (chars, listOfChars.length) }.sorted
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s mkString "")

  /**
   * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy wordOccurrences

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /**
   * Returns the power set by combining thisSet to prevPowerSet.
   * If no previous set is present please pass an empty List in prevPowerSet
   *
   * @param thisSubSet This particular subset
   * @param prevPowerSet Previous power set
   * @return Combined subset combining thisSubSet and prevPowerSet
   */
  private def getNextPowerSet(thisSubSet: Occurrences, prevPowerSet: List[Occurrences]) = {
    // single occurrences are also subsets by themselves
    val singleOccurrences = for { s <- thisSubSet } yield List(s)
    // Get subsets formed by combining thisSet with prevPowerSet
    val combinationOfOccurrences = (for {
      subSet <- prevPowerSet
      thisOccur <- thisSubSet
    } yield thisOccur :: subSet).distinct
    // Return the combination of the previous power set + single occurrences + combined ones
    prevPowerSet ::: singleOccurrences ::: combinationOfOccurrences
  }

  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(List())
    case x :: xs =>
      // Calculate all combination of occurrences for x
      val (char, times) = x
      val thisElemCombs: Occurrences = (for { o <- 1 to times } yield (char, o)).toList
      // Get subset for all elements apart from x
      val prevPowerSet: List[Occurrences] = combinations(xs)
      // generate subsets from previous subset along with x
      getNextPowerSet(thisElemCombs, prevPowerSet)
  }

  /**
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def subtractOccurrence(zs: Map[Char, Int], z: (Char, Int)): Map[Char, Int] = {
      val (key, value) = z
      val diff = zs(key) - value
      if (diff == 0) zs - key
      else zs.updated(key, diff)
    }
    (y.toMap foldLeft x.toMap)(subtractOccurrence)
  }.toList.sorted

  /**
   * Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def anagrams(l: Occurrences): List[Sentence] = l match {
      case List() => List(List())
      case _ =>
        for {
          o <- combinations(l)
          if dictionaryByOccurrences contains o
          s <- anagrams(subtract(l, o))
          w <- dictionaryByOccurrences(o)
        } yield w :: s
    }
    anagrams(sentenceOccurrences(sentence))
  }

  /** Main routine (small test) */
  def main(args: Array[String]): Unit = {
    val listOfAnagrams = sentenceAnagrams(List("Linux", "Rulez"))
    val listOfSentences = listOfAnagrams.map(_.mkString(" "))
    println(listOfSentences.mkString("\n"))
  }
}
