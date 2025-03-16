package exercice2

import java.time.LocalDate

case class OrganizationLaureate(
    id: Int,
    name: String,
    gender : StructureGender,
    award: Award
) extends Laureate with Organization
