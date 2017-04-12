package observatory


import observatory.Visualization._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers with Matchers {


  test("great-circle distance") {
    val loc1 = Location(52.205, 0.119)
    val loc2 = Location(48.857, 2.351)

    val res = gcDistance(loc1, loc2)

    BigDecimal(res).setScale(1, BigDecimal.RoundingMode.HALF_EVEN) shouldEqual 404.3
  }

  test("simple predict") {
    val loc = Location(52.205, 0.119)
    val testValue = 10d
    val val1 = Seq((loc, testValue))

    val res = predictTemperature(val1, loc)

    res shouldEqual testValue
  }

  test("multiple predict") {
    val loc = Location(52.205, 0.119)
    val testValue = 10d
    val val1 = Seq((loc, testValue), (Location(50.000, 0.000), 0d), (Location(55.000, 1.000), 20d))

    val res = predictTemperature(val1, loc)

    BigDecimal(res).setScale(1, BigDecimal.RoundingMode.HALF_EVEN) shouldEqual testValue
  }

  test("internet multiple predict") {
    val val1 = Seq((Location(10, 10), 10d), (Location(0, 0), 0d))

    val res = predictTemperature(val1, Location(5, 5))

    BigDecimal(res).setScale(1, BigDecimal.RoundingMode.HALF_EVEN) shouldEqual 5d
  }

  test("coursera color interpolation") {
    val val1 = Seq((0d, Color(255, 255, 255)), (10d, Color(0, 0, 0)))

    val res = interpolateColor(val1, 5)
    res shouldEqual Color(128, 128, 128)
  }

  test("simple color interpolation") {
    val val1 = List((0.0, Color(255, 0, 0)), (2.147483647E9, Color(0, 0, 255)))

    val res = interpolateColor(val1, 1.0737418235E9)
    res shouldEqual Color(128, 0, 128)
  }

  test("simple2 color interpolation") {
    val val1 = List((1.0,Color(255,0,0)), (9712.0,Color(0,0,255)))

    val res = interpolateColor(val1, 4856.5)
    res shouldEqual Color(128, 0, 128)
  }


  test("Color interpolation 1") {
    val scale = List((0.0, Color(0, 0, 255)))
    interpolateColor(scale, -0.5) shouldEqual Color(0, 0, 255)
    interpolateColor(scale, 0.5) shouldEqual Color(0, 0, 255)
    interpolateColor(scale, 0.0) shouldEqual Color(0, 0, 255)
  }

  test("Color interpolation 2") {
    // UNSORTED SCALE!!!
    val colors =
      List((60.0, Color(255, 255, 255)), (32.0, Color(255, 0, 0)),
        (12.0, Color(255, 255, 0)), (0.0, Color(0, 255, 255)),
        (-15.0, Color(0, 0, 255)), (-27.0, Color(255, 0, 255)),
        (-50.0, Color(33, 0, 107)), (-60.0, Color(0, 0, 0)))
    interpolateColor(colors, 12.0) shouldEqual Color(255, 255, 0)
    interpolateColor(colors, 62) shouldEqual Color(255, 255, 255)
    interpolateColor(colors, 6) shouldEqual Color(128, 255, 128)
  }


}
