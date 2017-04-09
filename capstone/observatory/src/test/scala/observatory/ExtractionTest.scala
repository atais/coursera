package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite with Matchers {

  val expected = Seq(
    (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
    (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
    (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
  )

  test("locateTemperatures from example data") {

    val temps = Extraction.locateTemperatures(2015, "/exstations.csv", "/ex2015.csv")

    temps.toList.sortBy(_._3) shouldEqual expected.sortBy(_._3)
  }

  test("average temp from example data") {

    val avgs = Extraction.locationYearlyAverageRecords(expected)

    val expectedAvgs = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )

    avgs.toList.sortBy(_._2) shouldEqual expectedAvgs.sortBy(_._2)
  }


}