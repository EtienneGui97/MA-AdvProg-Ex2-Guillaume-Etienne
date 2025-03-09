package exercice2

import java.util.Date

case class OrganizationLaureate(
    id: Int,
    name: String,
    founded: Option[Date],
    location: Option[Location],
    awards: List[Award]
) extends Laureate