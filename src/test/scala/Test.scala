package com.nicolaswinsten.watson

import org.apache.lucene.search.similarities._

import java.io.{FileOutputStream, PrintStream}
import java.nio.file.Paths

object Test {

  def main(args: Array[String]): Unit = {

    val indices = List(
      new Index("src/main/resources/plainIndex", PLAIN),
      new Index("src/main/resources/stemIndex", STEM),
      new Index("src/main/resources/lemmaIndex", LEMMA),
    )

    val simScorers = List(
      new BM25Similarity,
      new ClassicSimilarity,
      new BooleanSimilarity,
      new DFISimilarity(new IndependenceChiSquared),
      new LMDirichletSimilarity,
      new LMJelinekMercerSimilarity(0.7f),
    )

    System.setOut(new PrintStream(new FileOutputStream("scores.txt")))
    val runs = for (index <- indices; sim <- simScorers) yield (index, sim, Run(index, sim))

    val bestRun = runs maxBy (_._3)

    bestRun match {
      case (index, sim, score) =>
        println(s"The best performing model used index ${index.path} with $sim scoring avg P@1 $score")
    }
  }
}
