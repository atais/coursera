package observatory

import com.sksamuel.scrimage.Image

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val (v1, v2) = temperatures.map { case (l, t) =>
      val gcDist = gcDistance(l, location)
      if (gcDist <= 1) {
        (t, 0d)
      } else {
        val dist = math.pow(1.0 / gcDist, 2)
        (dist * t, dist)
      }
    }.fold((0d, 0d)) { case ((agg1, agg2), (x, y)) => (agg1 + x, agg2 + y) }

    val nonZero = if (v2 == 0) 1 else v2
    v1 / nonZero
  }

  private[observatory] def gcDistance(l1: Location, l2: Location): Double = {
    import math._
    val R = 6371e3
    val φ1 = l1.lat.toRadians
    val λ1 = l1.lon.toRadians
    val φ2 = l2.lat.toRadians

    val λ2 = l2.lon.toRadians
    val Δφ = φ2 - φ1
    val Δλ = λ2 - λ1

    val a = sin(Δφ / 2) * sin(Δφ / 2) + cos(φ1) * cos(φ2) * sin(Δλ / 2) * sin(Δλ / 2)
    val c = 2 * atan2(sqrt(a), sqrt(1 - a))
    R * c / 1000
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    ???
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    ???
  }

}

