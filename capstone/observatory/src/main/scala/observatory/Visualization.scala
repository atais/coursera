package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.util.{Failure, Success, Try}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    def wix(x: Location, xi: (Location, Temperature)): (Temperature, Double) = {
      val (xil, xit) = xi
      val gcDist = gcDistance(x, xil)
      if (gcDist <= 1) {
        (xit, 1d)
      } else {
        val dist = 1.0 / math.pow(gcDist, 2)
        (dist * xit, dist)
      }
    }

    val (v1, v2) = temperatures
      .map(wix(location, _))
      .fold((0d, 0d)) { case ((agg1, agg2), (x, y)) => (agg1 + x, agg2 + y) }

    val nonZero = if (v2 == 0) 1 else v2
    v1 / nonZero
  }

  private[observatory] def gcDistance(l1: Location, l2: Location): Double = {
    import math._
    val R = 6371
    val φ1 = l1.lat.toRadians
    val λ1 = l1.lon.toRadians
    val φ2 = l2.lat.toRadians
    val λ2 = l2.lon.toRadians

    val Δφ = φ2 - φ1
    val Δλ = λ2 - λ1

    val Δδ = if (l1 == l2) {
      0
    } else if (l1.lat == -l2.lat && l1.lon == -l2.lon) {
      Pi
    } else {
      acos(sin(φ1) * sin(φ2) + cos(φ1) * cos(φ2) * cos(Δλ))
    }
    R * Δδ
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val pMap = points.toMap
    val keys = pMap.keys.toSeq.sorted

    val x1 = keys.find(_ >= value).getOrElse(keys.max)
    val x0 = keys.reverse.find(_ <= value).getOrElse(keys.min)

    val y1 = pMap(x1)
    val y0 = pMap(x0)

    if (x0 == x1) y0
    else {
      val y1d = ColorD(y1)
      val y0d = ColorD(y0)
      Try {
        (y0d + ((y1d - y0d) / (x1 - x0) * (value - x0))).normalize
      } match {
        case Success(c) => c
        case Failure(t) =>
          println(t.toString)
          Color(0,0,0)
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val (w, h) = (360, 180)

    val pixels = for {
      x <- 0 until w
      y <- 0 until h
    } yield {
      val lat = h / 2 - y
      val lon = -1 * (w / 2 - x)
      val loc = Location(lat, lon)
      val temp = predictTemperature(temperatures, loc)
      val col = interpolateColor(colors, temp)
      Pixel(col.red, col.green, col.blue, 1)
    }

    Image(w, h, pixels.toArray)
  }

}

