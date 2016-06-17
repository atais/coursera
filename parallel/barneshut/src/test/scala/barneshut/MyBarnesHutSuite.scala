package barneshut

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection._

@RunWith(classOf[JUnitRunner])
class MyBarnesHutSuite extends FunSuite {

  test("'SectorMatrix.+=' should add a body at (64,27) to the correct bucket of a sector matrix of size 108 when precision is 12") {
    val body = new Body(5, 64, 27, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 109
    boundaries.maxY = 109
    val sm = new SectorMatrix(boundaries, 12)
    sm += body
    val res = sm(7, 2).size == 1 && sm(7, 2).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }

  test("Fork.insert(b) should insert recursively in the appropriate quadrant") {
    val e1 = Empty(10, 30, 10)
    val e2 = Empty(20, 30, 10)
    val e3 = Empty(10, 40, 10)
    val e4 = Empty(20, 40, 10)
    var f = Fork(e1, e2, e3, e4)

    f = f.insert(new Body(5, 10, 30, 0.1f, 0.1f))
    f = f.insert(new Body(5, 20, 30, 0.1f, 0.1f))
    f = f.insert(new Body(5, 20, 40, 0.1f, 0.1f))
    f = f.insert(new Body(5, 15, 35, 0.1f, 0.1f))

    assert(f.isInstanceOf[Fork])
    assert(f.nw.isInstanceOf[Fork])
    assert(f.nw.asInstanceOf[Fork].nw.isInstanceOf[Leaf])
    assert(f.nw.asInstanceOf[Fork].ne.isInstanceOf[Empty])
    assert(f.nw.asInstanceOf[Fork].sw.isInstanceOf[Empty])
    assert(f.nw.asInstanceOf[Fork].se.isInstanceOf[Leaf])
    assert(f.ne.isInstanceOf[Leaf])
    assert(f.sw.isInstanceOf[Empty])
    assert(f.se.isInstanceOf[Leaf])
  }

  test("'SectorMatrix.combine' should correctly combine two sector matrices of size 96 containing points: (12, 34), (23, 45), (56, 9), (8, 79), (5, 99)") {
    val body1 = new Body(5, 12, 34, 0.1f, 0.1f)
    val body2 = new Body(5, 23, 45, 0.1f, 0.1f)
    val body3 = new Body(5, 56, 9, 0.1f, 0.1f)
    val body4 = new Body(5, 8, 79, 0.1f, 0.1f)
    val body5 = new Body(5, 5, 99, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 4
    boundaries.minY = 4
    boundaries.maxX = 100
    boundaries.maxY = 100
    val sm1 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    val sm2 = new SectorMatrix(boundaries, SECTOR_PRECISION)

    def pos(b: Body) = (math.floor(b.x / boundaries.maxX * SECTOR_PRECISION).toInt, math.floor(b.y / boundaries.maxY * SECTOR_PRECISION).toInt)
    def getSm(b: Body, sm: SectorMatrix) = (sm.apply _).tupled.apply(pos(b))
    def getSm2(b: Body) = getSm(b, sm2)
    def getSm1(b: Body) = getSm(b, sm1)

    sm1 += body1
    sm1 += body2
    sm1 += body3
    sm2 += body4
    sm2 += body5

    assert(getSm1(body1).size == 1 && getSm1(body1).exists(_ == body1), s"$body1 not found in the right sector")
    assert(getSm1(body2).size == 1 && getSm1(body2).exists(_ == body2), s"$body2 not found in the right sector")
    assert(getSm1(body3).size == 1 && getSm1(body3).exists(_ == body3), s"$body3 not found in the right sector")
    assert(getSm2(body4).size == 1 && getSm2(body4).exists(_ == body4), s"$body4 not found in the right sector")
    assert(getSm2(body5).size == 1 && getSm2(body5).exists(_ == body5), s"$body5 not found in the right sector")

    val sm3 = sm1.combine(sm2)
    def getSm3(b: Body) = getSm(b, sm3)

    assert(getSm3(body1).size == 1 && getSm3(body1).exists(_ == body1), s"$body1 not found in the right sector")
    assert(getSm3(body2).size == 1 && getSm3(body2).exists(_ == body2), s"$body2 not found in the right sector")
    assert(getSm3(body3).size == 1 && getSm3(body3).exists(_ == body3), s"$body3 not found in the right sector")
    assert(getSm3(body4).size == 1 && getSm3(body4).exists(_ == body4), s"$body4 not found in the right sector")
    assert(getSm3(body5).size == 1 && getSm3(body5).exists(_ == body5), s"$body5 not found in the right sector")
  }
}



