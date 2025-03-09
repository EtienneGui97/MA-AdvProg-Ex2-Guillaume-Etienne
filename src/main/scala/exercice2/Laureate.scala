package exercice2

trait Laureate {
  def id: Int
  def award: Award

  def introduce(): Unit
}