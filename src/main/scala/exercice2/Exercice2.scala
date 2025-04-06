package exercice2

import scala.io.Source
import java.nio.charset.StandardCharsets
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.Try
import scala.language.reflectiveCalls

type hasPresentNobelPrize = { def presentNobelPrize(): Unit }
type allGender = PersonGender | StructureGender

//object CsvReader {
@main def main = //(args: Array[String]): Unit = {
    val filename = "C:/Users/guill/Downloads/06-NobelPrizeByWinner.csv"
    //val filename = Award.getClass.getResource("/06-NobelPrizeByWinner.csv").getPath

    val bufferedSource = Source.fromFile(filename, StandardCharsets.UTF_8.name())

    val laureates: List[Laureate] = bufferedSource.getLines().drop(1).toList.flatMap { line =>
        val rawCols = line.split(",").map(_.trim)
        val cols = reconstructQuotedValues(rawCols)
        createLaureat(cols)
    }
        
    bufferedSource.close()
    
    println("Total number of laureates per category:")
    val nbLaureatesByCategory = getNbLaureatesByCategory(laureates)
    nbLaureatesByCategory.foreach { case (category, count) =>
        println(s"- $category : $count")
    }

    println("\nTop 3 categories with the most laureates :")
    val topNCategories = getTopNCategories(laureates, 3)
    topNCategories.foreach { case (category, count) =>
        println(s"- $category : $count")
    }

    println("\nLaureats born in Switzerland :")
    val laureatesBornInSwitzerland = getLaureatesBornInCountry(laureates, "Switzerland")
    laureatesBornInSwitzerland.foreach{laureate =>
        laureate match
            case person: Laureate with Person =>
                person.introducePerson()
                writeNobelPrize(person)
    }

    println("\nNumber of female and male :")
    val (nbFemales, nbMales) = getNbMaleAndFemaleLaureates(laureates)
    println(s"- Female: $nbFemales")
    println(s"- Male: $nbMales")

    println("\nList of distinct institutions associated with a prize :")
    val distinctInstitutions = getDistinctInstitutions(laureates)
    distinctInstitutions.foreach { institution =>
        println(s"- $institution")
    }


/**
 * Exercise 5: FP & Collections
 */

 // Count the number of laureates per category by grouping and counting
def getNbLaureatesByCategory(laureates: List[Laureate]): Map[String, Int] =
    laureates
        .map(_.award.category)
        .groupBy(identity)
        .view.mapValues(_.size)
        .toMap


// Return the top N categories with the highest number of laureates by converting the map to a sequence,
// sort in descending by number, and selecting the first N elements.
def getTopNCategories(laureates: List[Laureate], n: Int): List[(String, Int)] =
    val nbLaureatesByCategory = getNbLaureatesByCategory(laureates)
    nbLaureatesByCategory
        .toSeq
        .sortBy { case (category, count) => -count }
        .take(n)
        .toList


// Return the list of laureates born in a country by filtering them on their born location.
def getLaureatesBornInCountry(laureates: List[Laureate], countryName: String): List[Laureate] =
    laureates.filter {
        case person: PersonLaureate => person.bornLocation.exists(_.country.equalsIgnoreCase(countryName))
        case _ => false
    }


// Count the number of male and female laureates using an accumulator for each gender
def getNbMaleAndFemaleLaureates(laureates: List[Laureate]): (Int, Int) =
    laureates.foldLeft((0, 0)) { case ((femaleCount, maleCount), laureate) =>
        laureate match
            case person: PersonLaureate if person.gender == PersonGender.Female =>
                (femaleCount + 1, maleCount)
            case person: PersonLaureate if person.gender == PersonGender.Male =>
                (femaleCount, maleCount + 1)
            case _ =>
                (femaleCount, maleCount)
    }


// Return the list of distinct institution by extracting and filtering them.
def getDistinctInstitutions(laureates: List[Laureate]): List[String] =
    laureates
        .flatMap(_.award.institution)
        .map(_.name)
        .filter(_.nonEmpty)
        .distinct


/**
 * Methods to parse data from csv
 */

def writeNobelPrize(laureat: hasPresentNobelPrize): Unit = 
    laureat.presentNobelPrize()


def createLaureat(columns: Array[String]): Option[Laureate] = 
    if (columns.length < 20) then None

    val gender : allGender = parseGender(columns(11))
    gender match 
        case personGender: PersonGender         => createPersonLaureat(columns, personGender)
        case structureGender: StructureGender   => createOrganizationLaureat(columns, structureGender)
    

def createPersonLaureat(columns: Array[String], gender: PersonGender): Option[PersonLaureate] =
    val id = Try(columns(0).toInt).toOption
    id.map { validId =>
        val firstname = columns(1)
        val surname = columns(2)
        val born = parseDate(columns(3))
        val died = parseDate(columns(4))
        val bornLocation = parseBornCountry(columns)
        val diedLocation = parseDiedCountry(columns)
        val award = parseAward(columns)

        PersonLaureate(validId, firstname, surname, born, died, bornLocation, diedLocation, gender, award)
    }
    

def createOrganizationLaureat(columns: Array[String], gender : StructureGender): Option[OrganizationLaureate] =
    val id = Try(columns(0).toInt).toOption
    id.map { validId =>
        val name = columns(1)
        val award = parseAward(columns)

        OrganizationLaureate(validId, name, gender, award)
    }


def parseDate(dateStr: String): Option[LocalDate] =
    val formatters = Seq(
        DateTimeFormatter.ofPattern("M/d/yyyy"),
        DateTimeFormatter.ofPattern("MM/dd/yyyy")
    )
    formatters.view.flatMap(fmt => Try(LocalDate.parse(dateStr, fmt)).toOption).headOption


def parseGender(genderStr: String): PersonGender | StructureGender  = 
    genderStr match 
        case "female" => PersonGender.Female
        case "male"  => PersonGender.Male
        case "org"    => StructureGender.Organization
        case _        => StructureGender.Other


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

