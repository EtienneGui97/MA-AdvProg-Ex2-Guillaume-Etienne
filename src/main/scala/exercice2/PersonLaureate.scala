package exercice2

import java.time.LocalDate

case class PersonLaureate(
    id: Int,
    firstname: String,
    surname: String,
    born: Option[LocalDate],
    died: Option[LocalDate],
    bornLocation: Option[Location],
    diedLocation: Option[Location],
    gender: Gender,
    award: Award
) extends Laureate {  
    override def introduce(): Unit = {
        val awardsInfo = s"${award.category} (${award.year.getOrElse("")})"

        val bornInfo = born.map(date => s"born on $date").getOrElse("")
        val diedInfo = died.map(date => s"died on $date").getOrElse("")
        
        val bornLocationInfo = bornLocation.map(loc => s"in ${loc.city}, ${loc.country}").getOrElse("")
        val diedLocationInfo = diedLocation.map(loc => s"in ${loc.city}, ${loc.country}").getOrElse("")

        val genderInfo = gender match {
            case Gender.Male => "He"
            case Gender.Female => "She"
            case null => ""
        }

        val deathMessage = if (died.isDefined) s" and died $diedInfo $diedLocationInfo" else ""

        println(s"$firstname $surname was $bornInfo $bornLocationInfo$deathMessage. $genderInfo received the Nobel Prize in $awardsInfo.")
    }
}