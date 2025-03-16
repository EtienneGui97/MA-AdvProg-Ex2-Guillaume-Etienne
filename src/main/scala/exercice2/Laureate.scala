package exercice2

trait Laureate :
    def id: Int
    def award: Award

    def presentNobelPrize(): Unit = 
        val awardsInfo = s"${award.category} (${award.year.getOrElse("")})"
        println(s"\tNobel Prize received in $awardsInfo.")