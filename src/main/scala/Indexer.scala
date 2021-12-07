package com.nicolaswinsten.watson

import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.{Document, Field, StringField, TextField}
import org.apache.lucene.index.{DirectoryReader, IndexWriter, IndexWriterConfig}
import org.apache.lucene.queryparser.classic.{QueryParser, QueryParserBase}
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.similarities.Similarity
import org.apache.lucene.store.MMapDirectory

import java.io.File
import java.nio.file.Path
import java.util.zip.GZIPInputStream
import scala.io.{BufferedSource, Codec, Source}

/**
 * Interface for indexing the documents and querying the resulting index
 * @param indexPath path to store the index
 * @param norm choice of text normalization (lemmatization, stemming, or none
 */
class Index(indexPath: String, norm: Norm, gobbleCats: Boolean = true) {
  val path = indexPath
  val index = new MMapDirectory(new File(indexPath).toPath)
  val analyzer = new StandardAnalyzer
  val config = new IndexWriterConfig(analyzer)

  // read the wikidata.gz file and index each document
  def createIndex(): Unit = {
    val inputStream = Thread.currentThread().getContextClassLoader.getResourceAsStream("wikidata.gz")
    val gzipFileSource: BufferedSource = Source.fromInputStream(new GZIPInputStream(inputStream))(Codec.ISO8859)

    val lines = gzipFileSource.getLines()
    lines.next() // skip first line its garbo

    val writer = new IndexWriter(index, config)
    var count = 0
    parseDocuments(lines) foreach { d =>
      writer.addDocument(d)
      count += 1
      if (count % 100 == 0) println(s"$count DOCS INDEXED")
    }
    writer.close()

    gzipFileSource.close()
    inputStream.close()
  }

  // takes the iterator of lines read from the gzip file and parses it into documents
  private def parseDocuments(lines: Iterator[String]): Iterator[Document] = {
    // the titles are enclosed in [[ ]]
    val titlePattern = "\\[\\[(.+)\\]\\]".r

    // find beginning of first document
    var nextTitle: Option[String] = {
      lines.find(titlePattern.pattern.asPredicate.test(_)) map {
        case titlePattern(title) => title
      }
    }

    // this iterator parses the next document on next()
    new Iterator[Document] {
      override def hasNext: Boolean = nextTitle.nonEmpty

      override def next(): Document = {
        val doc = new Document
        val content = new StringBuilder

        // pull out the title of the next document
        nextTitle match {
          case Some(title) =>
            doc.add(new StringField("title", title, Field.Store.YES))
            nextTitle = None // flag to None in case this was the last document
          case None => throw new NoSuchElementException
        }

        // grab the content of this document
        lines takeWhile {
          case titlePattern(title) =>
            // found title of next document, so store it and stop
            nextTitle = Some(title)
            false
          case _ => true // glob up any other lines
        } foreach { line =>
          if (!line.toLowerCase.startsWith("categories") || gobbleCats) {
            // skip categories info for doc if gobbleCats = false
            // otherwise add this line to the doc's content
            val line_ = norm.normalizer(cleanText(line))
            content.append(line_ + " ")
          }

        }

        doc.add(new TextField("content", content.result(), Field.Store.YES))
        doc
      }
    }
  }

  // make a query and return a list of matching documents zipped with their scores
  def query(queryText: String, limit: Int = 10, simScorer: Similarity): List[Document] = {
    val queryLemmas =
      QueryParserBase.escape(
        norm.normalizer(cleanText(queryText))
      )

    val reader = DirectoryReader.open(index)
    val searcher = new IndexSearcher(reader)
    searcher.setSimilarity(simScorer)

    val q = new QueryParser("content", analyzer).parse(queryLemmas)

    val foundDocs = searcher.search(q, limit)
    val res = foundDocs.scoreDocs.map(sd => searcher.doc(sd.doc)).toList
    reader.close()
    res

    // TODO maybe filter out pages whose titles appear in the query (those cant be the answer)
  }


  private def cleanText(text: String) = text.replaceAll("[=\\|\\,\\:]", " ")

  def close(): Unit = index.close()

}

sealed trait Norm {
  val normalizer: String => String
}
case object STEM extends Norm {
  override val normalizer: String => String = LanguageModel.stem
}
case object LEMMA extends Norm {
  override val normalizer: String => String = LanguageModel.lemmatize
}
case object PLAIN extends Norm {
  override val normalizer: String => String = identity
}
