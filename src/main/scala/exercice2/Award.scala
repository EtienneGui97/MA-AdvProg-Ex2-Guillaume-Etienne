package exercice2

import cats.Show
import cats.syntax.show._

case class Award(
    year: Option[Int],//jpc: sometimes there is no year?
    category: String, //jpc: can we Enumize the coategories?
    motivation: String,
    overallMotivation: String,
    share: Option[Int],
    institution: Option[Institution]
)

given Show[Award] with
  def show(a: Award): String =
    s"${a.category} (${a.year.map(_.toString).getOrElse("Unknown")})"