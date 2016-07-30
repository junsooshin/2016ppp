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
import util.control.Breaks._  // lets us use "breakable" and break" method

object BaseballSimulator {

	// 2D arrays of strings that contain the batting, pitching, and league
	// data; each row is an array of strings
	val batting = ArrayBuffer[Array[String]]()
	val pitching = ArrayBuffer[Array[String]]()
	val league = ArrayBuffer[Array[String]]()

	class player {
		var name: String = null
		var p1B: Double = 0
		var p2B: Double = 0
		var p3B: Double = 0
		var pHR: Double = 0
		var pBB: Double = 0
		var pIBB: Double = 0
		var pHBP: Double = 0
		var pSO: Double = 0
		var pSF: Double = 0
		var pSH: Double = 0
		var pNO: Double = 0
	}

	def checkArgsLength(args: Array[String]) {
		if (args.length == 0) {

			println("\nNo argument given\n")
			println("Type in two lineups as arguments; one team as one quote, "
					+ "players' names separated by commas, and pitcher's name "
					+ "as 10th player.\n")
			println("Ex: run \"Dexter Fowler, Kris Bryant,..., Jon Lester\" "
					+ "\"Mookie Betts, Dustin Pedroia,..., David Price\"\n")
			System.exit(0)
		} else if (args.length == 1) {
			println("\nOnly one argument given\n")
			println("Type in two lineups as arguments; one team as one quote, "
					+ "players' names separated by commas, and pitcher's name "
					+ "as 10th player.\n")
			println("Ex: run \"Dexter Fowler, Kris Bryant,..., Jon Lester\" "
					+ "\"Mookie Betts, Dustin Pedroia,..., David Price\"\n")
			System.exit(0)
		} else if (args.length == 2) {
			createDatabases()
			checkValidNames(args)
		} else {
			println("\nToo many arguments given\n")
			println("Type in two lineups as arguments; one team as one quote, "
					+ "players' names separated by commas, and pitcher's name "
					+ "as 10th player.\n")
			println("Ex: run \"Dexter Fowler, Kris Bryant,..., Jon Lester\" "
					+ "\"Mookie Betts, Dustin Pedroia,..., David Price\"\n")
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

	// This function first checks for the right number of players on the 
	// lineups and for the correct names. Then, it creates arrays for batters
	// and values for pitchers
	def checkValidNames(args: Array[String]) {
		val tmpLineup1 = args(0).split(",").map(_.trim)
		val tmpLineup2 = args(1).split(",").map(_.trim)
		// println(tmpLineup1.length)
		// println(tmpLineup2.length)

		if (tmpLineup1.length != 10) {
			println("\nNeeded 10 players on the 1st lineup but received "
					+ tmpLineup1.length)
			System.exit(0)
		}
		if (tmpLineup2.length != 10) {
			println("\nNeeded 10 players on the 2nd lineup but received "
					+ tmpLineup2.length)
			System.exit(0)
		}

		val batters1 = Array.fill(9)(new player)
		val pitcher1 = new player
		val batters2 = Array.fill(9)(new player)
		val pitcher2 = new player

		for (order <- 0 until tmpLineup1.length - 1) {
			breakable {
				for (numRow <- 0 until batting.length) {
					if (batting(numRow)(0) == tmpLineup1(order)) {
						batters1(order).name = tmpLineup1(order)
						break
					} else if (numRow == batting.length - 1) {
						// we just checked the last row and did not find the player
						println("\nPlayer named " + "\"" + tmpLineup1(order) 
						+ "\" was not found")
						System.exit(0)
					}
				}
			}
		}

		breakable {
			for (numRow <- 0 until pitching.length) {
				if (pitching(numRow)(0) == tmpLineup1(9)) {
					pitcher1.name = tmpLineup1(9)
					break
				} else if (numRow == pitching.length - 1) {
					println("\nPlayer named " + "\"" + tmpLineup1(9) 
					+ "\" was not found")
					System.exit(0)
				}
			}
		}

		for (order <- 0 until tmpLineup2.length - 1) {
			breakable {
				for (numRow <- 0 until batting.length) {
					if (batting(numRow)(0) == tmpLineup2(order)) {
						batters2(order).name = tmpLineup2(order)
						break
					} else if (numRow == batting.length - 1) {
						// we just checked the last row and did not find the player
						println("\nPlayer named " + "\"" + tmpLineup2(order) 
						+ "\" was not found")
						System.exit(0)
					}
				}
			}
		}

		breakable {
			for (numRow <- 0 until pitching.length) {
				if (pitching(numRow)(0) == tmpLineup2(9)) {
					pitcher2.name = tmpLineup2(9)
					break
				} else if (numRow == pitching.length - 1) {
					println("\nPlayer named " + "\"" + tmpLineup2(9) 
					+ "\" was not found")
					System.exit(0)
				}
			}
		}
	}

	def main(args: Array[String]) {
		checkArgsLength(args)
	}
}


