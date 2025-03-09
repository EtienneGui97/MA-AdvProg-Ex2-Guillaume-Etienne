package exercice2

import scala.io.Source
import java.nio.charset.StandardCharsets

object CsvReader {
  def main(args: Array[String]): Unit = {
    val filename = "C:/Users/guill/Downloads/06-NobelPrizeByWinner.csv"

    val bufferedSource = Source.fromFile(filename, StandardCharsets.UTF_8.name())

    for (line <- bufferedSource.getLines().drop(1)) {
      val rawCols = line.split(",").map(_.trim)
      val cols = reconstructQuotedValues(rawCols)

      if (cols.length > 11) { // Vérifie qu'on a assez de colonnes
        val gender = cols(11)
        println(gender)
      } else {
        println(s"⚠️ Ligne ignorée (pas assez de colonnes) : ${line}")
      }
    }

    bufferedSource.close()
  }


  def reconstructQuotedValues(columns: Array[String]): Array[String] = {
    val result = scala.collection.mutable.ArrayBuffer[String]()
    var temp = ""
    var insideQuotes = false

    for ((col, index) <- columns.zipWithIndex) {
      val startsWithQuote = col.startsWith("\"") || col.startsWith("\"\"\"")
      val endsWithQuote = col.endsWith("\"") || col.endsWith("\"\"\"")

      if (index == 0) {
        result.append(col.stripPrefix("\""))
      } else if (index == columns.length - 1) {
        result.append(col.stripSuffix("\""))
      } else if (startsWithQuote && !endsWithQuote) {
        temp = col
        insideQuotes = true
      } else if (endsWithQuote && insideQuotes) {
        temp += "," + col
        result.append(temp)
        insideQuotes = false
      } else if (insideQuotes) {
        temp += "," + col
      } else {
        result.append(col)
      }
    }
    result.toArray
  }
}
