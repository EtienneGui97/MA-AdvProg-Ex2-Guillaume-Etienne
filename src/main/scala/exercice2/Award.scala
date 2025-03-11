package exercice2

case class Award(
    year: Option[Int],//jpc: sometimes there is no year?
    category: String, //jpc: can we Enumize the coategories?
    motivation: String,
    overallMotivation: String,
    share: Option[Int],
    institution: Option[Institution]
)