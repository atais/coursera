package observatory

import java.time.LocalDate

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    val stations = scala.io.Source.fromInputStream(file(stationsFile))
      .getLines()
      .flatMap(l => {
        l.split(",") match {
          case Array(stn, wban, lat, lon) =>
            val loc = Location(lat.toDouble, lon.toDouble)
            val ws = WeatherStation(stn, Option(wban).getOrElse(""), loc)
            Option(ws)
          case _ => None
        }
      }).toList.groupBy(s => (s.stn, s.wban))

    val temperatures = scala.io.Source.fromInputStream(file(temperaturesFile))
      .getLines()
      .flatMap(l => {
        l.split(",") match {
          case Array(stn, wban, month, day, temp) => {
            val date = LocalDate.of(year, month.toInt, day.toInt)
            val tCel = fToC(temp.toDouble)
            val t = Temperature(stn, Option(wban).getOrElse(""), date, tCel)
            Option(t)
          }
          case _ => None
        }
      }).toList.groupBy(s => (s.stn, s.wban))

    stations.keySet.flatMap(k => {
      val station = stations.getOrElse(k, List.empty).headOption
      val temps = temperatures.getOrElse(k, List.empty)

      temps.flatMap(t => {
        station.map(s => {
          (t.date, s.loc, t.temp)
        })
      })
    })
  }

  private def fToC(f: Double) = BigDecimal((f - 32) / 1.8).setScale(1, BigDecimal.RoundingMode.HALF_EVEN).toDouble

  private def file(name: String) = this.getClass.getResourceAsStream(name)

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records
      .groupBy { case (date, loc, _) => (date.getYear, loc) }
      .map { case ((_, loc), group) =>
        val sum = group.map(_._3).sum
        (loc, sum / group.size)
      }
  }

}
