package observatory

import java.time.LocalDate

import org.apache.spark.sql.{Encoders, SparkSession}

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
    val spark = SparkSession.builder().config(Context.sparkConf).getOrCreate()
    import spark.implicits._

    val weatherSchema = Encoders.product[WeatherStation].schema
    val temperatureSchema = Encoders.product[Temperature].schema

    val weatherStations = spark.read.schema(weatherSchema).csv(file(stationsFile)).as[WeatherStation]
    val temperature = spark.read.schema(temperatureSchema).csv(file(temperaturesFile)).as[Temperature]

    weatherStations
      .na
      .drop(Seq("lat", "lon"))
      .join(temperature, $"w_stn" === $"t_stn" && ($"w_wban" === $"t_wban" || ($"w_wban".isNull && $"t_wban".isNull)))
      .map(r => {
        val month = r.getAs[Int](6)
        val day = r.getAs[Int](7)

        val lat = r.getAs[Double](2)
        val lon = r.getAs[Double](3)

        val c = fToC(r.getAs[Double](8))

        ((month, day), Location(lat, lon), c)
      })
      .collect()
      .map { case ((month, day), loc, temp) => (LocalDate.of(year, month, day), loc, temp) }
  }

  private def fToC(f: Double) = BigDecimal((f - 32) / 1.8).setScale(1, BigDecimal.RoundingMode.HALF_EVEN).toDouble

  private def file(name: String) = this.getClass.getResource(name).getFile

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
