package observatory

case class WeatherStation(w_stn: String, w_wban: String, lat: Double, lon: Double)

case class Temperature(t_stn: String, t_wban: String, month: Int, day: Int, temp: Double)

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

