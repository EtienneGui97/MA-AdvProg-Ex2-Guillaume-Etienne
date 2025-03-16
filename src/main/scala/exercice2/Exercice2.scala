package exercice2

import scala.io.Source
import java.nio.charset.StandardCharsets
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.Try
import scala.language.reflectiveCalls

type hasPresentNobelPrize = { def presentNobelPrize(): Unit }

//object CsvReader {
@main def main = //(args: Array[String]): Unit = {
    val filename = "C:/Users/guill/Downloads/06-NobelPrizeByWinner.csv"
    // val filename = Award.getClass.getResource("/06-NobelPrizeByWinner.csv").getPath

    val bufferedSource = Source.fromFile(filename, StandardCharsets.UTF_8.name())

    for (line <- bufferedSource.getLines().drop(1)) do
        val rawCols = line.split(",").map(_.trim)
        val cols = reconstructQuotedValues(rawCols)
        val laureat: Option[Laureate] = createLaureat(cols)

        laureat match
            case Some(person: Laureate with Person) =>
                person.introducePerson()
                writeNobelPrize(person)
            case Some(org: Laureate with Organization) => 
                org.introduceOrganization()
                writeNobelPrize(org)
            case Some(laureat: Laureate) => 
                laureat.presentNobelPrize()                    
            case None => 
                println("No laureate found")
        
    bufferedSource.close()


def writeNobelPrize(laureat: hasPresentNobelPrize): Unit = {
    laureat.presentNobelPrize()
}


def createLaureat(columns: Array[String]): Option[Laureate] = 
    if (columns.length < 20) then None

    else columns.lift(11) match 
        case Some("male") | Some("female") => createPersonLaureat(columns)
        case Some("org")                   => createOrganizationLaureat(columns)
        case _                             => None
    

def createPersonLaureat(columns: Array[String]): Option[PersonLaureate] = 
    val id = Try(columns(0).toInt).toOption

    id.map : validId =>
        val firstname = columns(1)
        val surname = columns(2)
        val born = parseDate(columns(3))
        val died = parseDate(columns(4))
        val bornLocation = parseBornCountry(columns)
        val diedLocation = parseDiedCountry(columns)
        val gender = parseGender(columns(11))
        val award = parseAward(columns)

        PersonLaureate(validId, firstname, surname, born, died, bornLocation, diedLocation, gender, award)
    

def createOrganizationLaureat(columns: Array[String]): Option[OrganizationLaureate] = 
    val id = Try(columns(0).toInt).toOption

    id.map : validId =>
        val name = columns(1)
        val award = parseAward(columns)

        OrganizationLaureate(validId, name, award)


def parseDate(dateStr: String): Option[LocalDate] =
    val formatters = Seq(
        DateTimeFormatter.ofPattern("M/d/yyyy"),
        DateTimeFormatter.ofPattern("MM/dd/yyyy")
    )
    formatters.view.flatMap(fmt => Try(LocalDate.parse(dateStr, fmt)).toOption).headOption


def parseGender(genderStr: String): Gender = 
    genderStr match 
        case "male"   => Gender.Male
        case "female" => Gender.Female


def parseAward(columns: Array[String]): Award = 
    val year: Option[Int] = Try(columns(12).toInt).toOption
    val category = columns(13)
    val motivation = columns(16)
    val overallMotivation = columns(14)
    val share = Try(columns(15).toInt).toOption
    val institution = parseInstitution(columns)

    Award(year, category, motivation, overallMotivation, share, institution)
    

def parseInstitution(columns: Array[String]): Option[Institution] = 
    val name = columns.lift(17).getOrElse("").trim
    val location = parseInstitutionLocation(columns)

    if (name.isEmpty && location.isEmpty) then None
    else Some(Institution(name, location))


def parseBornCountry(columns: Array[String]): Option[Location] = 
    val city = columns.lift(7).getOrElse("").trim
    val country = columns.lift(5).getOrElse("").trim
    val countryCode = columns.lift(6).getOrElse("").trim

    if (city.isEmpty && country.isEmpty && countryCode.isEmpty()) None
    else Some(Location(city, country, countryCode))
    

def parseDiedCountry(columns: Array[String]): Option[Location] = 
    val city = columns.lift(10).getOrElse("").trim
    val country = columns.lift(8).getOrElse("").trim
    val countryCode = columns.lift(9).getOrElse("").trim

    if (city.isEmpty && country.isEmpty && countryCode.isEmpty()) None
    else Some(Location(city, country, countryCode))


def parseInstitutionLocation(columns: Array[String]): Option[Location] = 
    val city = columns.lift(18).getOrElse("").trim
    val country = columns.lift(19).getOrElse("").trim
    val countryCode = ""

    if (city.isEmpty && country.isEmpty) None
    else Some(Location(city, country, countryCode))


def reconstructQuotedValues(columns: Array[String]): Array[String] = 
    val result = scala.collection.mutable.ArrayBuffer[String]()
    var temp = ""
    var insideQuotes = false

    for (col, index) <- columns.zipWithIndex do
        val startsWithQuote = col.startsWith("\"") || col.startsWith("\"\"\"")
        val endsWithQuote = col.endsWith("\"") || col.endsWith("\"\"\"")

        if (index == 0) then
            result.append(col.stripPrefix("\""))
        else if (index == columns.length - 1) then
            result.append(col.stripSuffix("\""))
        else if (startsWithQuote && !endsWithQuote) then
            temp = col
            insideQuotes = true
        else if (endsWithQuote && insideQuotes) then
            temp += "," + col
            result.append(temp)
            insideQuotes = false
        else if (insideQuotes) then
            temp += "," + col
        else
            result.append(col)
            
    result.toArray

