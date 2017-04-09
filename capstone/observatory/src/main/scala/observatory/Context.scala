package observatory

import org.apache.spark.SparkConf

/**
  * Created by michalsiatkowski on 08.04.2017.
  */
object Context {
  //set up the spark configuration and create contexts
  val sparkConf = new SparkConf().setAppName("observatory").setMaster("local")

}
