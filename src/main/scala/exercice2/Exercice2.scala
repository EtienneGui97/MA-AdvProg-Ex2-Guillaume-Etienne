package exercice2

import scala.io.Source
import java.nio.charset.StandardCharsets
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.Try

object CsvReader {
  def main(args: Array[String]): Unit = {
    val filename = "C:/Users/guill/Downloads/06-NobelPrizeByWinner.csv"

    val bufferedSource = Source.fromFile(filename, StandardCharsets.UTF_8.name())

    for (line <- bufferedSource.getLines().drop(1)) {
      val rawCols = line.split(",").map(_.trim)
      val cols = reconstructQuotedValues(rawCols)
      val laureat: Option[Laureate] = createLaureat(cols)
      if (laureat.isDefined) println(laureat.get.introduce())
    }

    bufferedSource.close()
  }

  def createLaureat(columns: Array[String]): Option[Laureate] = {
    if (columns.length < 20) return None

    columns.lift(11) match {
      case Some("male") | Some("female") => createPersonLaureat(columns)
      case Some("org")                   => createOrganizationLaureat(columns)
      case _                              => None
    }
  }

  def createPersonLaureat(columns: Array[String]): Option[PersonLaureate] = {
    val id = Try(columns(0).toInt).toOption

    id.map { validId =>
      val firstname = columns(1)
      val surname = columns(2)
      val born = parseDate(columns(3))
      val died = parseDate(columns(4))
      val bornLocation = parseBornCountry(columns)
      val diedLocation = parseDiedCountry(columns)
      val gender = parseGender(columns(11))
      val award = parseAward(columns)

      PersonLaureate(validId, firstname, surname, born, died, bornLocation, diedLocation, gender, award)
    }
  }

  def createOrganizationLaureat(columns: Array[String]): Option[OrganizationLaureate] = {
    val id = Try(columns(1).toInt).toOption

    id.map { validId =>
      val name = columns(1)
      val award = parseAward(columns)

      OrganizationLaureate(validId, name, award)
    }
  }

  def parseDate(dateStr: String): Option[LocalDate] = {
    val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
    Try(LocalDate.parse(dateStr, formatter)).toOption
  }

  def parseGender(genderStr: String): Gender = {
    genderStr match {
      case "male"   => Gender.Male
      case "female" => Gender.Female
    }
  }

  def parseBornCountry(columns: Array[String]): Option[Location] = {
    for {
      city <- columns.lift(7)
      country <- columns.lift(5)
      countryCode <- columns.lift(6)
    } yield Location(city, country, countryCode)
  }

  def parseDiedCountry(columns: Array[String]): Option[Location] = {
    for {
      city <- columns.lift(10)
      country <- columns.lift(8)
      countryCode <- columns.lift(9)
    } yield Location(city, country, countryCode)
  }

  def parseAward(columns: Array[String]): Award = {
    val year = Try(columns(12).toInt).getOrElse(0)
    val category = columns(13)
    val motivation = columns(16)
    val overallMotivation = columns(14)
    val share = Try(columns(15).toInt).getOrElse(0)
    val institution = parseInstitution(columns)

    Award(year, category, motivation, overallMotivation, share, institution)
  }

  def parseInstitution(columns: Array[String]): Institution = {
    val name = columns(17)
    val location = parseInstitutionLocation(columns)
    Institution(name, location)
  }

  def parseInstitutionLocation(columns: Array[String]): Location = {
    val city = columns(18)
    val country = columns(19)
    val countryCode = ""
    Location(city, country, countryCode)
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
