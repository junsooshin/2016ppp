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

	class scorecard {
		var batters = Array.fill(9)(new player)
		var oppPitcher = new player
		var runs = 0
		var outs = 0
		var bases = "000"
		var base1 = ""
		var base2 = ""
		var base3 = ""
		var currBatterOrder = 0
		var currPlay: Element[String] = Select(1.0 -> "")
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
		helper(batters1, pitcher1)
		helper(batters2, pitcher2)

		def helper(batters: Array[player], pitcher: player) {
			for (batter <- batters) {
				val plateAppB = battingData(batter.rowID)(4).toDouble
				batter.p1B  = battingData(batter.rowID)(6).toDouble  / plateAppB
				batter.p2B  = battingData(batter.rowID)(7).toDouble  / plateAppB
				batter.p3B  = battingData(batter.rowID)(8).toDouble  / plateAppB
				batter.pHR  = battingData(batter.rowID)(9).toDouble  / plateAppB
				batter.pTW  = battingData(batter.rowID)(24).toDouble / plateAppB
				batter.pSO  = battingData(batter.rowID)(14).toDouble / plateAppB
				batter.pBO  = battingData(batter.rowID)(23).toDouble / plateAppB
			}
		
			val plateAppP = pitchingData(pitcher.rowID)(5).toDouble
			pitcher.p1B  = pitchingData(pitcher.rowID)(9).toDouble  / plateAppP
			pitcher.p2B  = pitchingData(pitcher.rowID)(10).toDouble / plateAppP
			pitcher.p3B  = pitchingData(pitcher.rowID)(11).toDouble / plateAppP
			pitcher.pHR  = pitchingData(pitcher.rowID)(12).toDouble / plateAppP
			pitcher.pTW  = pitchingData(pitcher.rowID)(30).toDouble / plateAppP
			pitcher.pSO  = pitchingData(pitcher.rowID)(16).toDouble / plateAppP
			pitcher.pBO  = pitchingData(pitcher.rowID)(29).toDouble / plateAppP
		}

		val plateAppL = leagueData(1)(3).toDouble
		league.p1B  = leagueData(1)(5).toDouble  / plateAppL
		league.p2B  = leagueData(1)(6).toDouble / plateAppL
		league.p3B  = leagueData(1)(7).toDouble / plateAppL
		league.pHR  = leagueData(1)(8).toDouble / plateAppL
		league.pTW  = leagueData(1)(22).toDouble / plateAppL
		league.pSO  = leagueData(1)(13).toDouble / plateAppL
		league.pBO  = leagueData(1)(21).toDouble / plateAppL

		// for (batter <- batters2) {
		// 	val plateAppB = battingData(batter.rowID)(4).toDouble
		// 	println((battingData(batter.rowID)(6).toDouble  / plateAppB) +
		// 		(battingData(batter.rowID)(7).toDouble  / plateAppB) +
		// 	 	(battingData(batter.rowID)(8).toDouble  / plateAppB) +
		// 	 	(battingData(batter.rowID)(9).toDouble  / plateAppB) +
		// 	 	(battingData(batter.rowID)(24).toDouble / plateAppB) +
		// 	 	(battingData(batter.rowID)(14).toDouble / plateAppB) +
		// 	 	(battingData(batter.rowID)(23).toDouble / plateAppB))
		// }

		// val plateAppP = pitchingData(pitcher2.rowID)(5).toDouble
		// println((pitchingData(pitcher2.rowID)(9).toDouble  / plateAppP) +
		// (pitchingData(pitcher2.rowID)(10).toDouble / plateAppP) +
		// (pitchingData(pitcher2.rowID)(11).toDouble / plateAppP) +
		// (pitchingData(pitcher2.rowID)(12).toDouble / plateAppP) +
		// (pitchingData(pitcher2.rowID)(30).toDouble / plateAppP) +
		// (pitchingData(pitcher2.rowID)(16).toDouble / plateAppP) +
		// (pitchingData(pitcher2.rowID)(29).toDouble / plateAppP))

		// println((leagueData(1)(5).toDouble  / plateAppL) +
		// (leagueData(1)(6).toDouble / plateAppL) +
		// (leagueData(1)(7).toDouble / plateAppL) +
		// (leagueData(1)(8).toDouble / plateAppL) +
		// (leagueData(1)(22).toDouble / plateAppL) +
		// (leagueData(1)(13).toDouble / plateAppL) +
		// (leagueData(1)(21).toDouble / plateAppL))

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

	/*	This function creates two scorecards and calls playRegularInnings.
	 * 	It gets back a pair of scorecards when the game finishes.
	 */
	def playGame() {
		val initState1 = new scorecard
		val initState2 = new scorecard
		initState1.batters = batters1
		initState2.batters = batters2
		initState1.oppPitcher = pitcher2
		initState2.oppPitcher = pitcher1
		val pair = playRegularInnings(initState1, initState2, 0.0)
	}

	/*	This function calls playHalfInning with an appropriate scorecard,
	 *	depending on whether an inning is top or bottom.
	 *	It returns a pair of scorecards when the game finishes.
	 */
	def playRegularInnings(currState1: scorecard, currState2: scorecard, inn: Double): (scorecard, scorecard) = {
		var inning = inn
		if (inning <= 8) {  // top 9th isn't completed yet
			if (inning - (inning.floor) == 0) {  // top half; team 1 offense
				val nextState1 = playHalfInning(currState1)
				inning += 0.5
				playRegularInnings(nextState1, currState2, inning)
			} else {  // bottom half; team 2 offense
				val nextState2 = playHalfInning(currState2)
				inning += 0.5
				playRegularInnings(currState1, nextState2, inning)
			}
		} else if (inning == 8.5) {  // top 9th is completed
			if (currState1.runs < currState2.runs)  {  // team 2 has won
				val pair = (currState1, currState2)
				pair
			} else {  // team 2 hasn't won, so plays offense for the last time
				val nextState2 = playHalfInning(currState2)
				inning += 0.5
				playRegularInnings(currState1, nextState2, inning)
			}
		} else {  // bot 9th is completed; return the scorecards
			val pair = (currState1, currState2)
			pair
		}
	}

	/*
	class scorecard {
		val batters = Array.fill(9)(new player)
		val oppPitcher = new player
		var runs = 0
		var outs = 0
		var bases = "000"
		var base1 = null
		var base2 = null
		var base3 = null
		var currBatterOrder = 0
		var currPlay = null
	}
	*/

	/*	This function plays out an half inning until there are 3 outs. As it
	 *	runs, it updates the runs, bases, players on the bases, and the
	 *	players' total statistics. 
	 *  When it finishes, it resets the outs to 0 and bases to "000".
	 */
	def playHalfInning(currState: scorecard): scorecard = {

		if ((currState.outs == 0) || (currState.outs == 1) || (currState.outs == 2)) {
			if (currState.bases == "000") {
				if (playAtBat(currState).currPlay == "1B") {
					currState.bases == "100"
					currState.currBatterOrder
				} else if (playAtBat(currState).currPlay == "2B") {
					//
				} else if (playAtBat(currState).currPlay == "3B") {
					//
				} else if (playAtBat(currState).currPlay == "HR") {
					//
				} else if (playAtBat(currState).currPlay == "TW") {
					//
				} else if (playAtBat(currState).currPlay == "SO") {
					//
				}
			} else if (currState.bases == "100") {
				if (playAtBat(currState).currPlay == "1B") {
					//
				} else if (playAtBat(currState).currPlay == "2B") {
					//
				} else if (playAtBat(currState).currPlay == "3B") {
					//
				} else if (playAtBat(currState).currPlay == "HR") {
					//
				} else if (playAtBat(currState).currPlay == "TW") {
					//
				} else if (playAtBat(currState).currPlay == "SO") {
					//
				}
			} else if (currState.bases == "010") {
				if (playAtBat(currState).currPlay == "1B") {
					//
				} else if (playAtBat(currState).currPlay == "2B") {
					//
				} else if (playAtBat(currState).currPlay == "3B") {
					//
				} else if (playAtBat(currState).currPlay == "HR") {
					//
				} else if (playAtBat(currState).currPlay == "TW") {
					//
				} else if (playAtBat(currState).currPlay == "SO") {
					//
				}
			} else if (currState.bases == "001") {
				if (playAtBat(currState).currPlay == "1B") {
					//
				} else if (playAtBat(currState).currPlay == "2B") {
					//
				} else if (playAtBat(currState).currPlay == "3B") {
					//
				} else if (playAtBat(currState).currPlay == "HR") {
					//
				} else if (playAtBat(currState).currPlay == "TW") {
					//
				} else if (playAtBat(currState).currPlay == "SO") {
					//
				}
			} else if (currState.bases == "110") {
				if (playAtBat(currState).currPlay == "1B") {
					//
				} else if (playAtBat(currState).currPlay == "2B") {
					//
				} else if (playAtBat(currState).currPlay == "3B") {
					//
				} else if (playAtBat(currState).currPlay == "HR") {
					//
				} else if (playAtBat(currState).currPlay == "TW") {
					//
				} else if (playAtBat(currState).currPlay == "SO") {
					//
				}
			} else if (currState.bases == "101") {
				if (playAtBat(currState).currPlay == "1B") {
					//
				} else if (playAtBat(currState).currPlay == "2B") {
					//
				} else if (playAtBat(currState).currPlay == "3B") {
					//
				} else if (playAtBat(currState).currPlay == "HR") {
					//
				} else if (playAtBat(currState).currPlay == "TW") {
					//
				} else if (playAtBat(currState).currPlay == "SO") {
					//
				}
			} else if (currState.bases == "011") {
				if (playAtBat(currState).currPlay == "1B") {
					//
				} else if (playAtBat(currState).currPlay == "2B") {
					//
				} else if (playAtBat(currState).currPlay == "3B") {
					//
				} else if (playAtBat(currState).currPlay == "HR") {
					//
				} else if (playAtBat(currState).currPlay == "TW") {
					//
				} else if (playAtBat(currState).currPlay == "SO") {
					//
				}
			} else if (currState.bases == "111") {
				if (playAtBat(currState).currPlay == "1B") {
					//
				} else if (playAtBat(currState).currPlay == "2B") {
					//
				} else if (playAtBat(currState).currPlay == "3B") {
					//
				} else if (playAtBat(currState).currPlay == "HR") {
					//
				} else if (playAtBat(currState).currPlay == "TW") {
					//
				} else if (playAtBat(currState).currPlay == "SO") {
					//
				}
			}
		}

		if ((currState.outs == 2) && (playAtBat(currState).currPlay == "BO")) {
			if (currState.bases == "000") {
				
			} else if (currState.bases == "100") {

			} else if (currState.bases == "010") {
				
			} else if (currState.bases == "001") {
				
			} else if (currState.bases == "110") {
				
			} else if (currState.bases == "101") {
				
			} else if (currState.bases == "011") {
				
			} else if (currState.bases == "111") {
				
			}
		}

		if (currState.outs < 3) {
			playHalfInning(currState)
		} else {
			currState.outs = 0
			currState
		}
	}

	/*	This function considers the batter, pitcher, and league probabilities
	 * 	and uses the odds ratio method to calculate the probabilities for 
	 *	individual events occurring during an at-bat.
	 *	It updates the nextBatter.
	 */
	def playAtBat(currState: scorecard): scorecard = {

		val sumContactB = batters1(4).p1B + batters1(4).p2B + batters1(4).p3B + batters1(4).pHR + batters1(4).pBO
		val sumContactP = pitcher2.p1B + pitcher2.p2B + pitcher2.p3B + pitcher2.pHR + pitcher2.pBO
		val sumContactL = league.p1B + league.p2B + league.p3B + league.pHR + league.pBO

		val sumNotContactB = batters1(4).pTW + batters1(4).pSO
		val sumNotContactP = pitcher2.pTW + pitcher2.pSO
		val sumNotContactL = league.pTW + league.pSO

		val sumNotBOB = 1 - batters1(4).pBO
		val sumNotBOP = 1 - pitcher2.pBO
		val sumNotBOL = 1 - league.pBO

		val sumNot1BB = 1 - batters1(4).p1B
		val sumNot1BP = 1 - pitcher2.p1B
		val sumNot1BL = 1 - league.p1B

		val sumNot2BB = 1 - batters1(4).p2B
		val sumNot2BP = 1 - pitcher2.p2B
		val sumNot2BL = 1 - league.p2B

		val sumNot3BB = 1 - batters1(4).p3B
		val sumNot3BP = 1 - pitcher2.p3B
		val sumNot3BL = 1 - league.p3B

		val sumNotHRB = 1 - batters1(4).pHR
		val sumNotHRP = 1 - pitcher2.pHR
		val sumNotHRL = 1 - league.pHR

		val sumNotTWB = 1 - batters1(4).pTW
		val sumNotTWP = 1 - pitcher2.pTW
		val sumNotTWL = 1 - league.pTW

		val sumNotSOB = 1 - batters1(4).pSO
		val sumNotSOP = 1 - pitcher2.pSO
		val sumNotSOL = 1 - league.pSO


		val oddsContact = (sumContactB / (1 - sumContactB)) * (sumContactP / (1 - sumContactP)) / (sumContactL / (1 - sumContactL))
		val pContact = oddsContact / (1 + oddsContact)

		val oddsNotContact = (sumNotContactB / (1 - sumNotContactB)) * (sumNotContactP / (1 - sumNotContactP)) / (sumNotContactL / (1 - sumNotContactL))
		val pNotContact = oddsNotContact / (1 + oddsNotContact)

		val oddsNotBO = (sumNotBOB / (1 - sumNotBOB)) * (sumNotBOP / (1 - sumNotBOP)) / (sumNotBOL / (1 - sumNotBOL))
		val pNotBO = (oddsNotBO / (1 + oddsNotBO))

		val oddsNot1B = (sumNot1BB / (1 - sumNot1BB)) * (sumNot1BP / (1 - sumNot1BP)) / (sumNot1BL / (1 - sumNot1BL))
		val pNot1B = (oddsNot1B / (1 + oddsNot1B))

		val oddsNot2B = (sumNot2BB / (1 - sumNot2BB)) * (sumNot2BP / (1 - sumNot2BP)) / (sumNot2BL / (1 - sumNot2BL))
		val pNot2B = (oddsNot2B / (1 + oddsNot2B))

		val oddsNot3B = (sumNot3BB / (1 - sumNot3BB)) * (sumNot3BP / (1 - sumNot3BP)) / (sumNot3BL / (1 - sumNot3BL))
		val pNot3B = (oddsNot3B / (1 + oddsNot3B))

		val oddsNotHR = (sumNotHRB / (1 - sumNotHRB)) * (sumNotHRP / (1 - sumNotHRP)) / (sumNotHRL / (1 - sumNotHRL))
		val pNotHR = (oddsNotHR / (1 + oddsNotHR))

		val oddsNotTW = (sumNotTWB / (1 - sumNotTWB)) * (sumNotTWP / (1 - sumNotTWP)) / (sumNotTWL / (1 - sumNotTWL))
		val pNotTW = (oddsNotTW / (1 + oddsNotTW))

		val oddsNotSO = (sumNotSOB / (1 - sumNotSOB)) * (sumNotSOP / (1 - sumNotSOP)) / (sumNotSOL / (1 - sumNotSOL))
		val pNotSO = (oddsNotSO / (1 + oddsNotSO))


		val oddsBO = (batters1(4).pBO / (1 - batters1(4).pBO)) * (pitcher2.pBO / (1 - pitcher2.pBO)) / (league.pBO / (1 - league.pBO))
		val pBO = (oddsBO / (1 + oddsBO))

		val odds1B = (batters1(4).p1B / (1 - batters1(4).p1B)) * (pitcher2.p1B / (1 - pitcher2.p1B)) / (league.p1B / (1 - league.p1B))
		val p1B = (odds1B / (1 + odds1B))

		val odds2B = (batters1(4).p2B / (1 - batters1(4).p2B)) * (pitcher2.p2B / (1 - pitcher2.p2B)) / (league.p2B / (1 - league.p2B))
		val p2B = (odds2B / (1 + odds2B))

		val odds3B = (batters1(4).p3B / (1 - batters1(4).p3B)) * (pitcher2.p3B / (1 - pitcher2.p3B)) / (league.p3B / (1 - league.p3B))
		val p3B = (odds3B / (1 + odds3B)) 

		val oddsHR = (batters1(4).pHR / (1 - batters1(4).pHR)) * (pitcher2.pHR / (1 - pitcher2.pHR)) / (league.pHR / (1 - league.pHR))
		val pHR = (oddsHR / (1 + oddsHR)) 

		val oddsTW = (batters1(4).pTW / (1 - batters1(4).pTW)) * (pitcher2.pTW / (1 - pitcher2.pTW)) / (league.pTW / (1 - league.pTW))
		val pTW = (oddsTW / (1 + oddsTW))

		val oddsSO = (batters1(4).pSO / (1 - batters1(4).pSO)) * (pitcher2.pSO / (1 - pitcher2.pSO)) / (league.pSO / (1 - league.pSO))
		val pSO = (oddsSO / (1 + oddsSO))

		// println("pBO: " + pBO)
		// println("p1B: " + p1B)
		// println("p2B: " + p2B)
		// println("p3B: " + p3B)
		// println("pHR: " + pHR)
		// println("pTW: " + pTW)
		// println("pSO: " + pSO)

		// val totalProb = pBO + p1B + p2B + p3B + pHR + pTW + pSO
		// println("Matchup Total: " + totalProb)

		// println("-----")

		// println("pNotBO: " + pNotBO)
		// println("pNot1B: " + pNot1B)
		// println("pNot2B: " + pNot2B)
		// println("pNot3B: " + pNot3B)
		// println("pNotHR: " + pNotHR)
		// println("pNotTW: " + pNotTW)
		// println("pNotSO: " + pNotSO)

		// val totalNotProb = pNotBO + pNot1B + pNot2B + pNot3B + pNotHR + pNotTW + pNotSO
		// println("Matchup Not Total: " + totalNotProb)
		// println(totalProb + totalNotProb)

		// println("-----")

		// println("pBO: " + batters1(4).pBO)
		// println("p1B: " + batters1(4).p1B)
		// println("p2B: " + batters1(4).p2B)
		// println("p3B: " + batters1(4).p3B)
		// println("pHR: " + batters1(4).pHR)
		// println("pTW: " + batters1(4).pTW)
		// println("pSO: " + batters1(4).pSO)

		// val totalProb2 = batters1(4).pBO + batters1(4).p1B + batters1(4).p2B + batters1(4).p3B + batters1(4).pHR + batters1(4).pTW + batters1(4).pSO
		// println("Batter Total: " + totalProb2)

		currState.currPlay = Select(p1B -> "1B", p2B -> "2B", p3B -> "3B", pHR -> "HR", pTW -> "TW", pSO -> "SO", pBO -> "BO")

		currState.currBatterOrder += 1
		if (currState.currBatterOrder == 9) {
			currState.currBatterOrder = 0
		}
		currState
	}


	def main(args: Array[String]) {
		checkArgsLength(args)
		createDatabases()
		checkValidNames(args)
		initializeProbs()
		playGame()
	}
}


