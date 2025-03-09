package exercice2

import java.time.LocalDate

case class OrganizationLaureate(
    id: Int,
    name: String,
    award: Award
) extends Laureate {
    override def introduce(): Unit = {
        val awardsInfo = s"${award.category} (${award.year.getOrElse("")})"
        println(s"We are $name. We have received the Nobel Prize in $awardsInfo.")
    }
}