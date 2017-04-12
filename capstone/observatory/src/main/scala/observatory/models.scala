package observatory

import java.time.LocalDate

case class WeatherStation(stn: String, wban: String, loc: Location)

case class Temperature(stn: String, wban: String, date: LocalDate, temp: Double)

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int) {

  def +(t: Color): Color = Color(red + t.red, green + t.green, blue + t.blue)

  def -(t: Color): Color = Color(red - t.red, green - t.green, blue - t.blue)

  def +(t: ColorD): ColorD = ColorD(red + t.red, green + t.green, blue + t.blue)

  def -(t: ColorD): ColorD = ColorD(red - t.red, green - t.green, blue - t.blue)

  def /(t: Double): ColorD = ColorD(red / t, green / t, blue / t)

  def *(t: Double): ColorD = ColorD(red * t, green * t, blue * t)
}

case class ColorD(red: Double, green: Double, blue: Double) {
  def normalize: Color = Color(calmp(red), calmp(green), calmp(blue))

  def +(t: ColorD): ColorD = ColorD(red + t.red, green + t.green, blue + t.blue)

  def -(t: ColorD): ColorD = ColorD(red - t.red, green - t.green, blue - t.blue)

  def /(t: Double): ColorD = ColorD(red / t, green / t, blue / t)

  def *(t: Double): ColorD = ColorD(red * t, green * t, blue * t)

  protected def calmp(v: Double): Int = {
    if (v > 255d) 255
    else if (v < 0d) 0
    else math.round(v).toInt
  }
}

