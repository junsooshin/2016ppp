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
import java.io._  // for writing to a file
import scala.io.StdIn.{readLine}  // for prompting user input

object BaseballSimulator {

	// 2D arrays of strings that contain the batting, pitching, and league
	// data; each row is an array of strings
	val batting = ArrayBuffer[Array[String]]()
	val pitching = ArrayBuffer[Array[String]]()
	val league = ArrayBuffer[Array[String]]()

	abstract class player {
		val name: String
		val p1B: Double
		val p2B: Double
		val p3B: Double
		val pHR: Double
		val pBB: Double
		val pIBB: Double
		val pHBP: Double
		val pSO: Double
		val pSF: Double
		val pSH: Double
		val pNO: Double
	}
	abstract class batter extends player ()
	abstract class pitcher extends player ()
	abstract class league extends player ()

	def checkArgsLength(args: Array[String]) {
		if (args.length == 0) {
			println()
			println("No argument given")
			println()
			println("Type in two lineups as arguments; one team as one quote, "
					+ "players' names separated by commas, and pitcher's name "
					+ "as 10th player.")
			println()
			println("Ex: run \"Dexter Fowler, Kris Bryant,..., Jon Lester\" "
					+ "\"Mookie Betts, Dustin Pedroia,..., David Price\"")
			println()
			System.exit(0)
		} else if (args.length == 1) {
			println()
			println("Only one argument given") 
			println()
			println("Type in two lineups as arguments; one team as one quote, "
					+ "players' names separated by commas, and pitcher's name "
					+ "as 10th player.")
			println("Ex: run \"Dexter Fowler, Kris Bryant,..., Jon Lester\" "
					+ "\"Mookie Betts, Dustin Pedroia,..., David Price\"")
			println()
			System.exit(0)
		} else if (args.length == 2) {
			createDatabases()
			checkValidNames(args)
		} else {
			println()
			println("Too many arguments given")
			println()
			println("Type in two lineups as arguments; one team as one quote, "
					+ "players' names separated by commas, and pitcher's name "
					+ "as 10th player.")
			println("Ex: run \"Dexter Fowler, Kris Bryant,..., Jon Lester\" "
					+ "\"Mookie Betts, Dustin Pedroia,..., David Price\"")
			println()
			System.exit(0)
		}
	}

	//	This function reads in the csv files and fills up the 2D arrays of 
	// 	strings that contain the batting, pitching, and league data
	def createDatabases() {
		// helper function
		def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
	        try {
	            f(resource)
	        } finally {
	            resource.close()
	        }
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

	 //   	// print and write the data to check if everything went okay
	 //    // val pw = new PrintWriter(new File("hello.txt"))
	 //    for (numRow <- 0 until batting.length) {
	 //    	println()
	 //    	// pw.write("\n")
	 //    	for (numCol <- 0 until batting(0).length) {
	 //    		print(batting(numRow)(numCol) + ",")
	 //    		// pw.write("\"" + batting(numRow)(numCol) + "\"" + ",")
	 //    	}
	 //    }
	 //    println()
	 //    println()
		// // pw.close()
	}

	def checkValidNames(args: Array[String]) {
		println(args.split(","))
		// for (numRow <- 0 until batting.length) {

		// }
	}

	def main(args: Array[String]) {
		checkArgsLength(args)
	}
}


