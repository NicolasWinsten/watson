package com.nicolaswinsten.watson

import org.apache.lucene.search.similarities.Similarity

import scala.io.Source

object Run {

  // tests the given index on questions.txt
  // returns the percentage of correct answers given
  // set verbose = true, to see each query result
  // set word2vec = true to use word2vec to improve query result order
  def apply(index: Index, simScorer: Similarity, verbose: Boolean = false, word2vec: Boolean = false): Float = {
    println("==================================")
    println(s"TESTING INDEX ${index.path}")
    println(s"WITH $simScorer")

    var numTests = 0
    var correct = 0
    var correctIn10 = 0

    val questionsSource = Source.fromResource("questions.txt")
    questionsSource.getLines().sliding(3, 4).withPartial(false) foreach {
      case Seq(category, queryStr, answer) =>
        if (verbose) println(s"CAT: $category \nQUERY: $queryStr\nANSWER: $answer\nTOP 10:")

        val fullQuery = s"${category.toLowerCase.capitalize} $queryStr"
        val res = {
          val result = index.query(fullQuery, simScorer = simScorer)
          if (word2vec) LanguageModel.rank(queryStr, result)
          else result
        }


        if (verbose)
          res.zipWithIndex foreach { case (doc, pos) => println(s"${pos + 1}. ${doc.get("title")}") }

        numTests += 1
        res.find(doc => answer.toLowerCase contains doc.get("title").toLowerCase) foreach { foundAnswer =>
          correctIn10 += 1
          if (res.head == foundAnswer) {
            correct += 1
            if (verbose) println("correctly answered")
          } else if (verbose) println("answer is in top 10")
        }
        if (verbose) println("\n\n")
    }
    val score = correct.toFloat/numTests
    println(s"P@1: $score")
    println("==================================")
    questionsSource.close()
    score
  }
}
