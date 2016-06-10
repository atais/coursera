package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner])
class LineOfSightSuite extends FunSuite {

  import LineOfSight._

  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("lineOfSight parallel 1") {
    val output = new Array[Float](4)
    (1 to 4).foreach(i => {
      parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, i)
      assert(output.toList == List(0f, 1f, 4f, 4f), s"Error on $i")
    })
  }

  test("lineOfSight parallel 2") {
    val output = new Array[Float](5)
    (1 to 5).foreach(i => {
      parLineOfSight(Array[Float](0f, 7f, 2f, 33f, 48f), output, i)
      assert(output.toList == List(0f, 7f, 7f, 11f, 12f), s"Error on $i")
    })
  }

  test("lineOfSight parallel 3") {
    val output = new Array[Float](7)
    (1 to 7).foreach(i => {
      parLineOfSight(Array[Float](0f, 3f, 1f, 1f, 1f, 1f, 30f), output, i)
      assert(output.toList == List(0f, 3f, 3f, 3f, 3f, 3f, 5f), s"Error on $i")
    })
  }
}

