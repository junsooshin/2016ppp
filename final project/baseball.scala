
import scala.collection.mutable.ArrayBuffer
import com.cra.figaro.language._
import com.cra.figaro.library.compound._

object Baseball {

	def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
        try {
            f(resource)
        } finally {
            resource.close()
        }

	def main(args: Array[String]) {

	    // each row is an array of strings (the columns in the csv file)
	    val rows = ArrayBuffer[Array[String]]()

	    // (1) read the csv data
	    using(io.Source.fromFile("Batting.csv")) { source =>
	        for (line <- source.getLines) {
	            rows += line.split(",").map(_.trim)
	        }
	    }

	    // println(rows.length)
	    // println(rows(0).length)
	    // println(rows(1)(17))
		// println(rows(1).indexWhere( _ == "abercda01"))


	    // for (numRow <- 0 until rows.length) {
	    // 	println()
	    // 	for (numCol <- 0 until rows(0).length) {
	    // 		print(rows(numRow)(numCol)+",")
	    // 	}
	    // }

	    // // (2) print the results
	    // for (row <- rows) {
	    //     println(s"${row(0)},${row(1)},${row(2)},${row(3)},")
	    // }
	}
}