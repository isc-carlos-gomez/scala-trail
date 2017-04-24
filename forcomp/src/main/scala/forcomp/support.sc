package forcomp

import Anagrams._

object support {

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def occurrenceAnagrams(occurrences: Occurrences): List[Sentence] = occurrences match {
      case Nil => List(Nil)
      case _ => {
        for {
          subset <- combinations(occurrences) filter (dictionaryByOccurrences.contains(_))
          word <- dictionaryByOccurrences(subset)
          anagrams <- occurrenceAnagrams(subtract(occurrences, subset))
        } yield word :: anagrams
      }
    }
    occurrenceAnagrams(sentenceOccurrences(sentence))
  }                                               //> sentenceAnagrams: (sentence: forcomp.Anagrams.Sentence)List[forcomp.Anagrams
                                                  //| .Sentence]

  sentenceAnagrams(List("I", "am"))               //> res0: List[forcomp.Anagrams.Sentence] = List(List(aim))
  sentenceAnagrams(List("Ian"))                   //> res1: List[forcomp.Anagrams.Sentence] = List(List(Ian))
  sentenceAnagrams(List("Yes", "man"))            //> res2: List[forcomp.Anagrams.Sentence] = List(List(my, en, as), List(my, as, 
                                                  //| en), List(my, sane), List(my, Sean), List(yes, man), List(en, my, as), List(
                                                  //| en, as, my), List(men, say), List(as, my, en), List(as, en, my), List(say, m
                                                  //| en), List(man, yes), List(sane, my), List(Sean, my))
}