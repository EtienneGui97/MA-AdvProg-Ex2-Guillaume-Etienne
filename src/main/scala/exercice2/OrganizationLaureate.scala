package exercice2

import java.time.LocalDate

case class OrganizationLaureate(
    id: Int,
    name: String,
    founded: Option[LocalDate],
    location: Option[Location],
    awards: List[Award]
) extends Laureate