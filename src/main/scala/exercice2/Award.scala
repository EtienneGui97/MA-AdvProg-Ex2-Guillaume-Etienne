package exercice2

case class Award(
    year: Option[Int],
    category: String,
    motivation: String,
    overallMotivation: String,
    share: Option[Int],
    institution: Option[Institution]
)