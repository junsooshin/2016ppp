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
	val battingData = ArrayBuffer[Array[String]]()
	val pitchingData = ArrayBuffer[Array[String]]()
	val leagueData = ArrayBuffer[Array[String]]()

	// variables holding the player classes
	val batters1 = Array.fill(9)(new player)
	val batters2 = Array.fill(9)(new player)
	val pitcher1 = new player
	val pitcher2 = new player
	val league = new player

	class player {
		var name: String = null
		var rowID: Int = 0

		var p1B: Double = 0
		var p2B: Double = 0
		var p3B: Double = 0
		var pHR: Double = 0
		var pTW: Double = 0
		var pSO: Double = 0
		var pBO: Double = 0

		var tPA: Int = 0
		var t1B: Int = 0
		var t2B: Int = 0
		var t3B: Int = 0
		var tHR: Int = 0
		var tTW: Int = 0
		var tSO: Int = 0
		var tBO: Int = 0
	}

	class scoreboard {
		var offense = 1
		var score1 = 0
		var score2 = 0
		var inning = 0.0
		var outs = 0
		var bases = "000"
	}

	/* 	This function checks for the right number of arguments given from the
	 * 	command line input.
	 */
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
		} else if (args.length >= 3) {
			println("\nToo many arguments given\n")
			println("Type in two lineups as arguments; one team as one quote, "
					+ "players' names separated by commas, and pitcher's name "
					+ "as 10th player.\n")
			println("Ex: run \"Dexter Fowler, Kris Bryant,..., Jon Lester\" "
					+ "\"Mookie Betts, Dustin Pedroia,..., David Price\"\n")
			System.exit(0)
		}
	}

	/*	This function reads in the csv files and fills up the 2D arrays of 
	 *	strings that contain the batting, pitching, and league data
	 */
	def createDatabases() {
		// helper function
		def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
	        try {
	            f(resource)
	        } finally {
	            resource.close()
	        }
	    // read the batter data
	    using(io.Source.fromFile("FanGraphsbatting2015.csv")) { source =>
	        for (line <- source.getLines) {
	        	// split by commas, trim blank spaces left and right,
	        	// and remove blank spaces around the strings
	            battingData += line.split(",").map(_.trim).map(_.replace("\"", ""))
	        }
	    }
	    // read the pitcher data
	    using(io.Source.fromFile("BaseballReferencepitching2015.csv")) { source =>
	        for (line <- source.getLines) {
	        	// here, we remove the asterisks next to the player names
	            pitchingData += line.split(",").map(_.trim).map(_.replace("*", ""))
	        }
	    }
	    // read the league data
	    using(io.Source.fromFile("FanGraphsLeague2015.csv")) { source =>
	        for (line <- source.getLines) {
	            leagueData += line.split(",").map(_.trim).map(_.replace("\"", ""))
	        }
	    }

	 //   	// print and write the data to check if everything went okay
	 //    // val pw = new PrintWriter(new File("hello.txt"))
	 //    for (numRow <- 0 until battingData.length) {
	 //    	println()
	 //    	// pw.write("\n")
	 //    	for (numCol <- 0 until battingData(0).length) {
	 //    		print(battingData(numRow)(numCol) + ",")
	 //    		// pw.write("\"" + battingData(numRow)(numCol) + "\"" + ",")
	 //    	}
	 //    }
	 //    println()
	 //    println()
		// // pw.close()
	}

	/* 	This function first checks for the right number of players on the 
	 * 	lineups and for the correct names. Then, it assigns names and stores
	 *	the row numbers on the arrays for batters and on the immutable 
	 *	variables for pitchers
	 */
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

		for (order <- 0 until tmpLineup1.length - 1) {
			breakable {
				for (numRow <- 0 until battingData.length) {
					if (battingData(numRow)(0) == tmpLineup1(order)) {
						batters1(order).name = tmpLineup1(order)
						batters1(order).rowID = numRow
						break
					} else if (numRow == battingData.length - 1) {
						// we just checked the last row and did not find the player
						println("\nPlayer named " + "\"" + tmpLineup1(order) 
						+ "\" was not found")
						System.exit(0)
					}
				}
			}
		}

		breakable {
			for (numRow <- 0 until pitchingData.length) {
				if (pitchingData(numRow)(0) == tmpLineup1(9)) { 
					pitcher1.name = tmpLineup1(9)
					pitcher1.rowID = numRow
					break
				} else if (numRow == pitchingData.length - 1) {
					println("\nPlayer named " + "\"" + tmpLineup1(9) 
					+ "\" was not found")
					System.exit(0)
				}
			}
		}

		for (order <- 0 until tmpLineup2.length - 1) {
			breakable {
				for (numRow <- 0 until battingData.length) {
					if (battingData(numRow)(0) == tmpLineup2(order)) {
						batters2(order).name = tmpLineup2(order)
						batters2(order).rowID = numRow
						break
					} else if (numRow == battingData.length - 1) {
						// we just checked the last row and did not find the player
						println("\nPlayer named " + "\"" + tmpLineup2(order) 
						+ "\" was not found")
						System.exit(0)
					}
				}
			}
		}

		breakable {
			for (numRow <- 0 until pitchingData.length) {
				if (pitchingData(numRow)(0) == tmpLineup2(9)) {
					pitcher2.name = tmpLineup2(9)
					pitcher2.rowID = numRow
					break
				} else if (numRow == pitchingData.length - 1) {
					println("\nPlayer named " + "\"" + tmpLineup2(9) 
					+ "\" was not found")
					System.exit(0)
				}
			}
		}
	}

	/*	This function initializes the probabilities in the batter, pitcher,
	 *	and league classes
	 */
	def initializeProbs() {
		println(batters1(0).rowID)

		for (batter1 <- batters1) {
			val plateApp1 = battingData(batter1.rowID)(4).toDouble
			batter1.p1B  = battingData(batter1.rowID)(6).toDouble  / plateApp1
			batter1.p2B  = battingData(batter1.rowID)(7).toDouble  / plateApp1
			batter1.p3B  = battingData(batter1.rowID)(8).toDouble  / plateApp1
			batter1.pHR  = battingData(batter1.rowID)(9).toDouble  / plateApp1
			batter1.pTW  = battingData(batter1.rowID)(24).toDouble / plateApp1
			batter1.pSO  = battingData(batter1.rowID)(14).toDouble / plateApp1
			batter1.pBO  = battingData(batter1.rowID)(23).toDouble / plateApp1

			// println((battingData(batter1.rowID)(6).toDouble  / plateApp1) +
			// (battingData(batter1.rowID)(7).toDouble  / plateApp1) +
			// (battingData(batter1.rowID)(8).toDouble  / plateApp1) +
			// (battingData(batter1.rowID)(9).toDouble  / plateApp1) +
			// (battingData(batter1.rowID)(24).toDouble / plateApp1) +
			// (battingData(batter1.rowID)(14).toDouble / plateApp1) +
			// (battingData(batter1.rowID)(23).toDouble / plateApp1))
		}

		for (batter2 <- batters2) {
			val plateApp2 = battingData(batter2.rowID)(4).toDouble
			batter2.p1B  = battingData(batter2.rowID)(6).toDouble  / plateApp2
			batter2.p2B  = battingData(batter2.rowID)(7).toDouble  / plateApp2
			batter2.p3B  = battingData(batter2.rowID)(8).toDouble  / plateApp2
			batter2.pHR  = battingData(batter2.rowID)(9).toDouble  / plateApp2
			batter2.pTW  = battingData(batter2.rowID)(24).toDouble / plateApp2
			batter2.pSO  = battingData(batter2.rowID)(14).toDouble / plateApp2
			batter2.pBO  = battingData(batter2.rowID)(23).toDouble / plateApp2
		}

		val plateApp3 = pitchingData(pitcher1.rowID)(5).toDouble
		pitcher1.p1B  = pitchingData(pitcher1.rowID)(9).toDouble  / plateApp3
		pitcher1.p2B  = pitchingData(pitcher1.rowID)(10).toDouble / plateApp3
		pitcher1.p3B  = pitchingData(pitcher1.rowID)(11).toDouble / plateApp3
		pitcher1.pHR  = pitchingData(pitcher1.rowID)(12).toDouble / plateApp3
		pitcher1.pTW  = pitchingData(pitcher1.rowID)(30).toDouble / plateApp3
		pitcher1.pSO  = pitchingData(pitcher1.rowID)(16).toDouble / plateApp3
		pitcher1.pBO  = pitchingData(pitcher1.rowID)(29).toDouble / plateApp3

		val plateApp4 = pitchingData(pitcher2.rowID)(5).toDouble
		pitcher2.p1B  = pitchingData(pitcher2.rowID)(9).toDouble  / plateApp4
		pitcher2.p2B  = pitchingData(pitcher2.rowID)(10).toDouble / plateApp4
		pitcher2.p3B  = pitchingData(pitcher2.rowID)(11).toDouble / plateApp4
		pitcher2.pHR  = pitchingData(pitcher2.rowID)(12).toDouble / plateApp4
		pitcher2.pTW  = pitchingData(pitcher2.rowID)(30).toDouble / plateApp4
		pitcher2.pSO  = pitchingData(pitcher2.rowID)(16).toDouble / plateApp4
		pitcher2.pBO  = pitchingData(pitcher2.rowID)(29).toDouble / plateApp4

		val plateApp5 = leagueData(1)(3).toDouble
		league.p1B  = leagueData(1)(5).toDouble  / plateApp5
		league.p2B  = leagueData(1)(6).toDouble / plateApp5
		league.p3B  = leagueData(1)(7).toDouble / plateApp5
		league.pHR  = leagueData(1)(8).toDouble / plateApp5
		league.pTW  = leagueData(1)(22).toDouble / plateApp5
		league.pSO  = leagueData(1)(13).toDouble / plateApp5
		league.pBO  = leagueData(1)(21).toDouble / plateApp5

		// println((pitchingData(pitcher2.rowID)(9).toDouble  / plateApp4) +
		// (pitchingData(pitcher2.rowID)(10).toDouble / plateApp4) +
		// (pitchingData(pitcher2.rowID)(11).toDouble / plateApp4) +
		// (pitchingData(pitcher2.rowID)(12).toDouble / plateApp4) +
		// (pitchingData(pitcher2.rowID)(30).toDouble / plateApp4) +
		// (pitchingData(pitcher2.rowID)(16).toDouble / plateApp4) +
		// (pitchingData(pitcher2.rowID)(29).toDouble / plateApp4))

		// println((leagueData(1)(5).toDouble  / plateApp5) +
		// (leagueData(1)(6).toDouble / plateApp5) +
		// (leagueData(1)(7).toDouble / plateApp5) +
		// (leagueData(1)(8).toDouble / plateApp5) +
		// (leagueData(1)(22).toDouble / plateApp5) +
		// (leagueData(1)(13).toDouble / plateApp5) +
		// (leagueData(1)(21).toDouble / plateApp5))

		
	}
	/*
	class scoreboard {
		var offense = 1
		var nextBatter1 = 0
		var nextBatter2 = 0
		var score1 = 0
		var score2 = 0
		var inning = 0.0
		var outs = 0
		var bases = "000"
	}
	*/

	def playGame() {
		val initState = new scoreboard
		val currState = playHalfInning(initState)
		println(currState.inning)
	}

	def playHalfInning(currState: scoreboard): scoreboard = {
		if (currState.inning <= 8.5) {
			val nextState = playAtBat(currState)
			playHalfInning(nextState)
		} else {
			currState
		}
	}

	def playAtBat(currState: scoreboard): scoreboard = {
		if (currState.outs == 0) {
			if (currState.bases == "000") {
				if ()
			} else if (currState.bases == "100") {
				//
			} else if (currState.bases == "010") {
				//
			} else if (currState.bases == "001") {
				//
			} else if (currState.bases == "110") {
				//
			} else if (currState.bases == "101") {
				//
			} else if (currState.bases == "011") {
				//
			} else if (currState.bases == "111") {
				//
			}
		} else if (currState.outs == 1) {

		}
	}

	def main(args: Array[String]) {
		checkArgsLength(args)
		createDatabases()
		checkValidNames(args)
		initializeProbs()
		playGame()
	}
}


