package exercice2

import java.time.LocalDate
import cats.Show
import cats.syntax.show._

case class OrganizationLaureate(
    id: Int,
    name: String,
    gender : StructureGender,
    award: Award
) extends Laureate with Organization


given Show[OrganizationLaureate] with
  def show(org: OrganizationLaureate): String =
    s"[ID: ${org.id}, Name: ${org.name}, Gender: ${org.gender.show}, Award: ${org.award.show}]"

