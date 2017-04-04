package timeusage

import org.apache.spark.sql.types.{DoubleType, StringType, StructType}
import org.apache.spark.sql.{Column, Row}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with Matchers with BeforeAndAfterAll {

  test("dfSchema") {
    val schema = TimeUsage.dfSchema(List("a", "b", "c"))
    val expected = new StructType()
      .add("a", StringType, false)
      .add("b", DoubleType, false)
      .add("c", DoubleType, false)

    schema shouldEqual expected
  }

  test("row") {
    val row = TimeUsage.row(List("a", "0.5", "1"))
    val expected = Row("a", 0.5d, 1d)

    row shouldEqual expected
  }

  test("classified columns") {
    /*
    * 1. “primary needs” activities (sleeping, eating, etc.). These are the columns starting with “t01”, “t03”, “t11”,
    *      “t1801” and “t1803”.
    * 2. working activities. These are the columns starting with “t05” and “t1805”.
    * 3. other activities (leisure). These are the columns starting with “t02”, “t04”, “t06”, “t07”, “t08”, “t09”,
    *      “t10”, “t12”, “t13”, “t14”, “t15”, “t16” and “t18” (those which are not part of the previous groups only).
    */
    val names = List("t0102", "t0301", "t110012", "t1801123132", "t1803132123")
    val classified = TimeUsage.classifiedColumns(names)

    classified._1 shouldEqual names.map(new Column(_))
  }

  test("sql") {
    val (columns, initDf) = TimeUsage.read("/timeusage/atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = TimeUsage.classifiedColumns(columns)
    val summaryDf = TimeUsage.timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)

    val expected = TimeUsage.timeUsageGrouped(summaryDf).collect
    val sql = TimeUsage.timeUsageGroupedSql(summaryDf).collect

    sql shouldEqual expected
  }

  test("typed") {
    val (columns, initDf) = TimeUsage.read("/timeusage/atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = TimeUsage.classifiedColumns(columns)
    val summaryDf = TimeUsage.timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)

    val expected = TimeUsage.timeUsageGrouped(summaryDf)
      .collect()
      .map(r => TimeUsageRow(r.getAs[String](0), r.getAs[String](1), r.getAs[String](2),
        r.getAs[Double](3), r.getAs[Double](4), r.getAs[Double](5)))

    val typedSummary = TimeUsage.timeUsageSummaryTyped(summaryDf)
    val typed = TimeUsage.timeUsageGroupedTyped(typedSummary).collect()

    typed shouldEqual expected
  }


}
