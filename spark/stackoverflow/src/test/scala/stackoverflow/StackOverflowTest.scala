package stackoverflow

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class StackOverflowTest extends FunSuite with BeforeAndAfterAll with Matchers {


  test("test from description #1") {
    val file = StackOverflow.getClass.getResource("stackoverflow.csv")

    val lines = StackOverflow.sc.textFile(file.getPath)
    val raw = StackOverflow.rawPostings(lines)
    val grouped = StackOverflow.groupedPostings(raw)
    val scored = StackOverflow.scoredPostings(grouped).cache()
    val vectors = StackOverflow.vectorPostings(scored)

//    val expect = Array(((1, 6, None, None, 140, Some("CSS")), 67),
//      ((1, 42, None, None, 155, Some("PHP")), 89),
//      ((1, 72, None, None, 16, Some("Ruby")), 3),
//      ((1, 126, None, None, 33, Some("Java")), 30),
//      ((1, 174, None, None, 38, Some("C#")), 20
//      ))

//        scored.intersect(expect) shouldEqual expect
    scored.count shouldEqual 2121822
    vectors.count shouldEqual 2121822
  }


}
