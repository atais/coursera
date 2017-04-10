package observatory


import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers with Matchers {


  test("great-circle distance") {
    val loc1 = Location(52.205, 0.119)
    val loc2 = Location(48.857, 2.351)

    val res = Visualization.gcDistance(loc1, loc2)

    BigDecimal(res).setScale(1, BigDecimal.RoundingMode.HALF_EVEN) shouldEqual 404.3
  }

  test("simple predict") {
    val loc = Location(52.205, 0.119)
    val testValue = 10d
    val val1 = Seq((loc, testValue))

    val res = Visualization.predictTemperature(val1, loc)

    res shouldEqual testValue
  }

}
