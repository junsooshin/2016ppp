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
		var score1 = 0
		var score2 = 0
		var inning = 0.0
		var outs = 0
		var bases = "000"
		var offense = 1
		var nextBatter1 = 0
		var nextBatter2 = 0
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

		// println(batters1(4).name)
		// println(batters1(4).p1B)
		// println(batters1(4).p2B)
		// println(batters1(4).p3B)
		// println(batters1(4).pHR)
		// println(batters1(4).pTW)
		// println(batters1(4).pSO)
		// println(batters1(4).pBO)

		// println(pitcher2.name)
		// println(pitcher2.p1B)
		// println(pitcher2.p2B)
		// println(pitcher2.p3B)
		// println(pitcher2.pHR)
		// println(pitcher2.pTW)
		// println(pitcher2.pSO)
		// println(pitcher2.pBO)
	}

	// def playGame() {
	// 	val initState = new scoreboard
	// 	val currState = playRegularInnings(initState)
	// 	println(currState.inning)
	// }

	// def playRegularInnings(currState: scoreboard): scoreboard = {
	// 	if (currState.inning <= 8.5) {  // TODO: take care of the 9th and extra innings
	// 		val nextState = playHalfInning(currState)
	// 		playRegularInnings(nextState)
	// 	} else {
	// 		currState
	// 	}
	// }

	// def playHalfInning(currState: scoreboard): scoreboard = {
	// 	if (currState.outs == 0) {
	// 		if (currState.bases == "000") {
	// 			if ()
	// 		} else if (currState.bases == "100") {
	// 			//
	// 		} else if (currState.bases == "010") {
	// 			//
	// 		} else if (currState.bases == "001") {
	// 			//
	// 		} else if (currState.bases == "110") {
	// 			//
	// 		} else if (currState.bases == "101") {
	// 			//
	// 		} else if (currState.bases == "011") {
	// 			//
	// 		} else if (currState.bases == "111") {
	// 			//
	// 		}
	// 	} else if (currState.outs == 1) {

	// 	} else if (currState.outs == 2) {

	// 	} else if (currState.outs == 3) {
	// 		currState
	// 	}
	// }

	
	// class player {
	// 	var name: String = null
	// 	var rowID: Int = 0

	// 	var p1B: Double = 0
	// 	var p2B: Double = 0
	// 	var p3B: Double = 0
	// 	var pHR: Double = 0
	// 	var pTW: Double = 0
	// 	var pSO: Double = 0
	// 	var pBO: Double = 0

	// 	var tPA: Int = 0
	// 	var t1B: Int = 0
	// 	var t2B: Int = 0
	// 	var t3B: Int = 0
	// 	var tHR: Int = 0
	// 	var tTW: Int = 0
	// 	var tSO: Int = 0
	// 	var tBO: Int = 0
	// }

	// class scoreboard {
	// 	var score1 = 0
	// 	var score2 = 0
	// 	var inning = 0.0
	// 	var outs = 0
	// 	var bases = "000"
	// 	var offense = 1
	// 	var nextBatter1 = 0
	// 	var nextBatter2 = 0
	// }
	

	/*	This function considers the batter, pitcher, and league probabilities
	 * 	and uses the odds ratio method to calculate the probabilities for 
	 *	individual events occurring during an at-bat.
	 *	It updates the nextBatter.
	 */
	def playAtBat(currState: scoreboard): scoreboard = {
		val sumContactB = batters1(4).p1B + batters1(4).p2B + batters1(4).p3B + batters1(4).pHR + batters1(4).pBO
		val sumContactP = pitcher2.p1B + pitcher2.p2B + pitcher2.p3B + pitcher2.pHR + pitcher2.pBO
		val sumContactL = league.p1B + league.p2B + league.p3B + league.pHR + league.pBO

		val sumNotContactB = batters1(4).pTW + batters1(4).pSO
		val sumNotContactP = pitcher2.pTW + pitcher2.pSO
		val sumNotContactL = league.pTW + league.pSO

		val oddsContact = (sumContactB / (1 - sumContactB)) * (sumContactP / (1 - sumContactP)) / (sumContactL / (1 - sumContactL))
		val pContact = oddsContact / (1 + oddsContact)
		val oddsNotContact = (sumNotContactB / (1 - sumNotContactB)) * (sumNotContactP / (1 - sumNotContactP)) / (sumNotContactL / (1 - sumNotContactL))
		val pNotContact = oddsNotContact / (1 + oddsNotContact)

		val sumNotBOB = 1 - batters1(4).pBO
		val sumNotBOP = 1 - pitcher2.pBO
		val sumNotBOL = 1 - league.pBO

		val oddsBO = (batters1(4).pBO / (1 - batters1(4).pBO)) * (pitcher2.pBO / (1 - pitcher2.pBO)) / (league.pBO / (1 - league.pBO))
		val pBO = (oddsBO / (1 + oddsBO))
		val oddsNotBO = (sumNotBOB / (1 - sumNotBOB)) * (sumNotBOP / (1 - sumNotBOP)) / (sumNotBOL / (1 - sumNotBOL))
		val pNotBO = (oddsNotBO / (1 + oddsNotBO))

		val sumNot1BB = 1 - batters1(4).p1B
		val sumNot1BP = 1 - pitcher2.p1B
		val sumNot1BL = 1 - league.p1B

		val odds1B = (batters1(4).p1B / (1 - batters1(4).p1B)) * (pitcher2.p1B / (1 - pitcher2.p1B)) / (league.p1B / (1 - league.p1B))
		val p1B = (odds1B / (1 + odds1B))
		val oddsNot1B = (sumNot1BB / (1 - sumNot1BB)) * (sumNot1BP / (1 - sumNot1BP)) / (sumNot1BL / (1 - sumNot1BL))
		val pNot1B = (oddsNot1B / (1 + oddsNot1B))

		val sumNot2BB = 1 - batters1(4).p2B
		val sumNot2BP = 1 - pitcher2.p2B
		val sumNot2BL = 1 - league.p2B

		val odds2B = (batters1(4).p2B / (1 - batters1(4).p2B)) * (pitcher2.p2B / (1 - pitcher2.p2B)) / (league.p2B / (1 - league.p2B))
		val p2B = (odds2B / (1 + odds2B))
		val oddsNot2B = (sumNot2BB / (1 - sumNot2BB)) * (sumNot2BP / (1 - sumNot2BP)) / (sumNot2BL / (1 - sumNot2BL))
		val pNot2B = (oddsNot2B / (1 + oddsNot2B))

		val sumNot3BB = 1 - batters1(4).p3B
		val sumNot3BP = 1 - pitcher2.p3B
		val sumNot3BL = 1 - league.p3B

		val odds3B = (batters1(4).p3B / (1 - batters1(4).p3B)) * (pitcher2.p3B / (1 - pitcher2.p3B)) / (league.p3B / (1 - league.p3B))
		val p3B = (odds3B / (1 + odds3B)) 
		val oddsNot3B = (sumNot3BB / (1 - sumNot3BB)) * (sumNot3BP / (1 - sumNot3BP)) / (sumNot3BL / (1 - sumNot3BL))
		val pNot3B = (oddsNot3B / (1 + oddsNot3B))

		val sumNotHRB = 1 - batters1(4).pHR
		val sumNotHRP = 1 - pitcher2.pHR
		val sumNotHRL = 1 - league.pHR

		val oddsHR = (batters1(4).pHR / (1 - batters1(4).pHR)) * (pitcher2.pHR / (1 - pitcher2.pHR)) / (league.pHR / (1 - league.pHR))
		val pHR = (oddsHR / (1 + oddsHR)) 
		val oddsNotHR = (sumNotHRB / (1 - sumNotHRB)) * (sumNotHRP / (1 - sumNotHRP)) / (sumNotHRL / (1 - sumNotHRL))
		val pNotHR = (oddsNotHR / (1 + oddsNotHR))

		val sumNotTWB = 1 - batters1(4).pTW
		val sumNotTWP = 1 - pitcher2.pTW
		val sumNotTWL = 1 - league.pTW

		val oddsTW = (batters1(4).pTW / (1 - batters1(4).pTW)) * (pitcher2.pTW / (1 - pitcher2.pTW)) / (league.pTW / (1 - league.pTW))
		val pTW = (oddsTW / (1 + oddsTW))
		val oddsNotTW = (sumNotTWB / (1 - sumNotTWB)) * (sumNotTWP / (1 - sumNotTWP)) / (sumNotTWL / (1 - sumNotTWL))
		val pNotTW = (oddsNotTW / (1 + oddsNotTW))

		val sumNotSOB = 1 - batters1(4).pSO
		val sumNotSOP = 1 - pitcher2.pSO
		val sumNotSOL = 1 - league.pSO

		val oddsSO = (batters1(4).pSO / (1 - batters1(4).pSO)) * (pitcher2.pSO / (1 - pitcher2.pSO)) / (league.pSO / (1 - league.pSO))
		val pSO = (oddsSO / (1 + oddsSO))
		val oddsNotSO = (sumNotSOB / (1 - sumNotSOB)) * (sumNotSOP / (1 - sumNotSOP)) / (sumNotSOL / (1 - sumNotSOL))
		val pNotSO = (oddsNotSO / (1 + oddsNotSO))

		println("pBO: " + pBO)
		println("p1B: " + p1B)
		println("p2B: " + p2B)
		println("p3B: " + p3B)
		println("pHR: " + pHR)
		println("pTW: " + pTW)
		println("pSO: " + pSO)
		val totalProb = pBO + p1B + p2B + p3B + pHR + pTW + pSO
		println("Total: " + totalProb)

		currState
	}


	def main(args: Array[String]) {
		checkArgsLength(args)
		createDatabases()
		checkValidNames(args)
		initializeProbs()
		val currState = new scoreboard
		playAtBat(currState)
		// playGame()
	}
}


