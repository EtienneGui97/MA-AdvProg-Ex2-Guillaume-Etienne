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
    gender: PersonGender,
    award: Award
) extends Laureate with Person