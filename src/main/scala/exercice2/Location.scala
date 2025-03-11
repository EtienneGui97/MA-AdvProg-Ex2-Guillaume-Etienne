package exercice2

//jpc: We could eventuqally create classes for cities and/or countries so we known also form which continent thy come or we can count by country, etc.
// jpc: in some cases countries change, Germany -> Poland, perhaps in the future we can also track these changes
case class Location(
    city: String, 
    country: String, 
    countryCode: String
)
