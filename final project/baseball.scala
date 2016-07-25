/*	title: baseball.scala
 *	name: Jun Soo Shin
 *	date: 2 August 2016
 *	note: Final project for the Probabilistic Programming Praktikum class.
 *
 *		  
 */

import scala.collection.mutable.ArrayBuffer
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import java.io._

object Baseball {

	//	This function reads in the csv files and creates 2D arrays of strings
	// 	that contain the batting, pitching, and league data
	def createDatabases() {

		// helper function
		def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
	        try {
	            f(resource)
	        } finally {
	            resource.close()
	        }

	    // each row is an array of strings
	    val batting = ArrayBuffer[Array[String]]()
	    val pitching = ArrayBuffer[Array[String]]()
	    val league = ArrayBuffer[Array[String]]()

	    // read the batter data
	    using(io.Source.fromFile("FanGraphsBatting2015.csv")) { source =>
	        for (line <- source.getLines) {
	        	// split by commas, trim blank spaces left and right,
	        	// and remove blank spaces around the strings
	            batting += line.split(",").map(_.trim).map(_.replace("\"", ""))
	        }
	    }

	    // read the pitcher data
	    using(io.Source.fromFile("BaseballReferencePitching2015.csv")) { source =>
	        for (line <- source.getLines) {
	        	// here, we remove the asterisks next to the player names
	            pitching += line.split(",").map(_.trim).map(_.replace("*", ""))
	        }
	    }

	    // read the league data
	    using(io.Source.fromFile("FanGraphsLeague2015.csv")) { source =>
	        for (line <- source.getLines) {
	            league += line.split(",").map(_.trim).map(_.replace("\"", ""))
	        }
	    }

	   	// print and write the data to check if everything went okay
	    // val pw = new PrintWriter(new File("hello.txt"))
	    for (numRow <- 0 until pitching.length) {
	    	println()
	    	// pw.write("\n")
	    	for (numCol <- 0 until pitching(0).length) {
	    		print(pitching(numRow)(numCol) + ",")
	    		// pw.write("\"" + batting(numRow)(numCol) + "\"" + ",")
	    	}
	    }
	    println()
	    println()
		// pw.close()
	}

	def main(args: Array[String]) {
		createDatabases()
	}
}


