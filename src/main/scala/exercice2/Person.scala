package exercice2

import java.time.LocalDate

trait Person:
    def firstname: String
    def surname: String
    def born: Option[LocalDate]
    def died: Option[LocalDate]
    def bornLocation: Option[Location]
    def diedLocation: Option[Location]
    def gender: Gender

    def introducePerson(): Unit =
        val genderInfo = gender match
            case Gender.Male   => "M"
            case Gender.Female => "F"
            case _             => ""

        val bornInfo = born.map(date => s"Born on $date").getOrElse("")
        val diedInfo = died.map(date => s"Died on $date").getOrElse("")
        
        val bornLocationInfo = bornLocation.map(loc => s"in ${loc.city}, ${loc.country}.").getOrElse("")
        val diedLocationInfo = diedLocation.map(loc => s"in ${loc.city}, ${loc.country}.").getOrElse("")
        
        val birthMessage = if (born.isDefined) s"$bornInfo $bornLocationInfo" else ""
        val deathMessage = if (died.isDefined) s"$diedInfo $bornLocationInfo." else ""

        println(s"$firstname $surname ($genderInfo). $birthMessage $deathMessage")
