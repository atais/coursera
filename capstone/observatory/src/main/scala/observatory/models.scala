package observatory

import java.time.LocalDate

case class WeatherStation(stn: String, wban: String, loc: Location)

case class Temperature(stn: String, wban: String, date: LocalDate, temp: Double)

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

