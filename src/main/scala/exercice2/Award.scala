package exercice2

case class Award(
    year: Int,
    category: Category,
    motivation: Option[String],
    overallMotivation: Option[String],
    share: Int,
    institution: Institution
)