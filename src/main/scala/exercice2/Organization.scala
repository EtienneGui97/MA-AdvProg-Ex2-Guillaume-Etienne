package exercice2

import java.time.LocalDate

trait Organization:
    def name: String
    def gender : StructureGender

    def introduceOrganization(): Unit = println(s"We are $name.")