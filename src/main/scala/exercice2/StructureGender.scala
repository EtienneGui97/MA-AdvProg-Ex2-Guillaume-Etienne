package exercice2

import cats.Show

enum StructureGender :
    case Organization, Other

    
given Show[StructureGender] with
  def show(g: StructureGender): String = g match
    case StructureGender.Organization => "Organization"
    case StructureGender.Other => "Other"