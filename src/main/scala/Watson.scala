package com.nicolaswinsten.watson

import org.apache.lucene.search.similarities.LMDirichletSimilarity

object Watson {
  def main(args: Array[String]): Unit = {
    val index = new Index("src/main/resources/lemmaIndex", LEMMA)
    Run(index, new LMDirichletSimilarity, verbose = true)
    index.close()
  }
}
