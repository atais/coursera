package observatory

import java.time.LocalDate

import scala.math.BigDecimal.RoundingMode

case class WeatherStation(stn: String, wban: String, loc: Location)

case class TemperatureC(stn: String, wban: String, date: LocalDate, temp: Double)

/**
  * Introduced in Week 1. Represents a location on the globe.
  *
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double)

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  *
  * @param x    X coordinate of the tile
  * @param y    Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int)

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  *
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int)

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  *
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double)

/**
  * Introduced in Week 2. Represents an RGB color.
  *
  * @param red   Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue  Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int)

case class ColorD(red: Double, green: Double, blue: Double) {

  def +(o: ColorD): ColorD = this ++ o

  def -(o: ColorD): ColorD = this ++ (o ** (-1))

  def *(d: Double): ColorD = this ** d

  def /(d: Double): ColorD = this ** (1 / d)

  private def ++(o: ColorD): ColorD = {
    val r = red + o.red
    val g = green + o.green
    val b = blue + o.blue
    ColorD(r, g, b)
  }

  private def **(d: Double): ColorD = {
    val r = red * d
    val g = green * d
    val b = blue * d
    ColorD(r, g, b)
  }

  def normalize: Color = Color(calmp(red * 255), calmp(green * 255), calmp(blue * 255))

  private def calmp(i: BigDecimal) = {
    if (i > 255) 255
    else if (i < 0) 0
    else i.setScale(0, RoundingMode.HALF_UP).toIntExact
  }

}

object ColorD {
  def apply(c: Color): ColorD = new ColorD(c.red / 255, c.green / 255, c.blue / 255)
}


