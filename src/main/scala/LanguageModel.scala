package com.nicolaswinsten.watson

import edu.stanford.nlp.pipeline.StanfordCoreNLP
import org.apache.lucene.document.Document
import org.tartarus.snowball.ext.PorterStemmer

import java.util.Properties
import java.util.zip.GZIPInputStream
import scala.annotation.tailrec
import scala.io.{BufferedSource, Codec, Source}

/**
 * Contains methods to stem and lemmatize text as well as a way to transform text to a vector using word2vec
 */
object LanguageModel {

  val props = new Properties()
  props.setProperty("annotators", "tokenize,ssplit,pos,lemma")
  val pipeline = new StanfordCoreNLP(props)

  // returns lemmatized text
  def lemmatize(text: String): String = {
    val doc = pipeline.processToCoreDocument(text)
    val lemmas = new StringBuilder
    doc.tokens().forEach {
      tok => if (tok.lemma().length > 1) lemmas.append(" " + tok.lemma())
    }
    lemmas.result()
  }

  // returns stemmed text
  private val stemmer = new PorterStemmer
  def stem(text: String): String = {
    val res = new StringBuilder
    text.split("\\s+") foreach { word =>
      stemmer.setCurrent(word.stripSuffix("."))
      stemmer.stem()
      res.append(" " + stemmer.getCurrent)
    }
    res.result()
  }

  // returns mapping of the given set of words to their word2vec vectors.
  // this function reads through the glove.gz file to search for the pairings
  def getWordVecs(terms: Set[String]): Map[String, Vector[Float]] = {
    type Word2VecMap = Map[String, Vector[Float]]

    val inputStream = Thread.currentThread().getContextClassLoader.getResourceAsStream("glove.gz")
    val gzipFileSource: BufferedSource = Source.fromInputStream(new GZIPInputStream(inputStream))(Codec.ISO8859)

    // create iterator of word-vector pairings from the gzip file
    val wordVecs = gzipFileSource.getLines() map (_.split("\\s+")) collect {
      case Array(word, vec @ _*) if terms contains word => word -> vec.map(_.toFloat).toVector
    }

    // recursive function that digs through the vecs iterator just enough to find all the terms wanted
    @tailrec
    def collectVecs(wordBag: Set[String], wordVecAcc: Word2VecMap): Word2VecMap = {
      if (wordBag.isEmpty || !wordVecs.hasNext) wordVecAcc
      else {
        val (word, vec) = wordVecs.next()
        if (wordBag contains word) collectVecs(wordBag - word, wordVecAcc + (word -> vec))
        else collectVecs(wordBag, wordVecAcc)
      }
    }

    val wordVecMap = collectVecs(terms, Map.empty)
    gzipFileSource.close()
    inputStream.close()

    terms -- wordVecMap.keySet foreach (term => println(s"wordVec not found for \'$term\'"))
    wordVecMap
  }

  // rank the list of documents against the query using word2vec
  def rank(queryText: String, docs: List[Document]): List[Document] = {
    def textToTerms(text: String) = text.split("\\s+") filter (_.length > 2)

    def avgVec(vecs: Seq[Vector[Float]], length: Int): Vector[Float] = {
      val numVecs = vecs.length
      val avgs = 0 until length map { i =>
        (vecs map (_(i))).sum / numVecs
      }
      avgs.toVector
    }

    val queryTerms = textToTerms(lemmatize(queryText))

    // pairings of docs to a list of their first 100 content words
    val docTerms = docs map { doc => (doc, textToTerms(doc.get("content")) take 100)}

    val terms = (queryTerms ++ (docTerms flatMap (_._2))).toSet
    val wordVecMap = getWordVecs(terms)

    // returns the average vector of the given words' vectors
    def termsToVec(terms: Seq[String]): Vector[Float] = avgVec(
      terms collect {
        case term if wordVecMap.contains(term) => wordVecMap(term)
      }
      , 300
    )

    val queryVec = termsToVec(queryTerms)

    // magnitude of a vector
    def mag(vec: Vector[Float]): Double = math.sqrt((vec map (x => x*x)).sum)

    // (proportional) cosine similarity of vec to the query vec
    def cosineSim(docVec: Vector[Float]): Double = {
      require(docVec.length == queryVec.length)
      val dotProd = (docVec zip queryVec map (p => p._1 * p._2)).sum
      dotProd / mag(docVec)
    }

    val docsRanked = docTerms map {
      case (doc, terms) => (doc, cosineSim(termsToVec(terms)))
    } sortBy(_._2) map (_._1)
    docsRanked.reverse
  }
}
