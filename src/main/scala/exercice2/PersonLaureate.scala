package exercice2

import java.util.Date

case class PersonLaureate(
    id: Int,
    firstname: String,
    surname: String,
    born: Option[Date],
    died: Option[Date],
    bornLocation: Option[Location],
    diedLocation: Option[Location],
    gender: Gender,
    awards: List[Award]
) extends Laureate