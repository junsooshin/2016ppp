/*	title: baseball.scala
 *	name: Jun Soo Shin
 *	date: 2 August 2016
 *	note: Final project for the Probabilistic Programming Praktikum class.
 *
 *		  Goal is to create a single game baseball simulator based on the
 *		  given two lineups of players and their past performance data.
 *
 */

import scala.collection.mutable.ArrayBuffer
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import util.control.Breaks._  // for "breakable" and break" method
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.algorithm.factored.MPEVariableElimination
import com.cra.figaro.algorithm.factored.beliefpropagation.MPEBeliefPropagation
import com.cra.figaro.algorithm.sampling.MetropolisHastingsAnnealer
import com.cra.figaro.algorithm.sampling.ProposalScheme
import com.cra.figaro.algorithm.sampling.Schedule
import scala.language.reflectiveCalls

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

	case class player (
		var name: String = null,
		var rowID: Int = 0,

		var p1B: Double = 0, // probabilities to simulate a play
		var p2B: Double = 0,
		var p3B: Double = 0,
		var pHR: Double = 0,
		var pTW: Double = 0,
		var pSO: Double = 0,
		var pBO: Double = 0,

		var t1B: Int = 0,  // stats for box score
		var t2B: Int = 0,
		var t3B: Int = 0,
		var tHR: Int = 0,
		var tTW: Int = 0,
		var tSO: Int = 0,
		var tBO: Int = 0,

		var tPA: Int = 0,
		var tR: Int = 0,
		var tRBI: Int = 0
	)

	case class scorecard (
		batters: Array[player],
		oppPitcher: player,
		runs: Int,
		outs: Int,
		bases: String, // bases occupied; 0 is empty, 1 is full; 1st 2nd 3rd
		base1: Int,  // indicates who is on 1st base by batter order
		base2: Int,
		base3: Int,
		currBatterOrder: Int
	) 
	// {
	// 	def updateBatter(): scorecard {

	// 	}
	// }

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
	    using(io.Source.fromFile("FanGraphsBatting2015.csv")) { source =>
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
	}

	/* 	This function first checks for the right number of players on the 
	 * 	lineups and for the correct names. Then, it assigns names and stores
	 *	the row numbers on the arrays for batters and on the immutable 
	 *	variables for pitchers
	 */
	def checkValidNames(args: Array[String]) {
		val tmpLineup1 = args(0).split(",").map(_.trim)
		val tmpLineup2 = args(1).split(",").map(_.trim)

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
	}

	/*	This function creates two scorecards and calls playInnings.
	 * 	It gets back a pair of scorecards when the game finishes.
	 */
	def playGame(currBatterOrder: Int): Element[(scorecard, scorecard, Boolean)] = {
		val initState1 = scorecard(batters1, pitcher2, 0, 0, "000", -1, -1, -1, 0)
		// setting up the bot 9th bases loaded situation
		// 4th batter on team 2 at the plate
		def getRunnerOrder (currBatterOrder: Int, base: Int): Int = {
			(currBatterOrder + 9 - base) % 9
		}
		val initState2 = scorecard(batters2, pitcher1, 0, 1, "111", getRunnerOrder(currBatterOrder, 1), getRunnerOrder(currBatterOrder, 2), getRunnerOrder(currBatterOrder, 3), currBatterOrder)

		val pair = playInnings(initState1, initState2, 8.5)

		for {
			result <- pair
		}
		yield {
			val (result1, result2) = result
			if (result1.runs < result2.runs) (result1, result2, true)
			else (result1, result2, false)
		}
	}

	/*	This function calls playHalfInning with an appropriate scorecard,
	 *	depending on whether an inning is top or bottom.
	 *	It returns a pair of scorecards when the game finishes.
	 */
	def playInnings(currState1: scorecard, currState2: scorecard, inn: Double): Element[(scorecard, scorecard)] = {

		var inning = inn

		// println()
		// println("Inning: " + inning)
		// println("Score: " + currState1.runs + "-" + currState2.runs)

		if (inning <= 8) {  // top 9th isn't completed yet
			if (inning - (inning.floor) == 0) {  // top half; team 1 offense
				val nextState1 = playHalfInning(currState1)
				inning += 0.5
				for {
					state1 <- nextState1
					pair <- playInnings(state1, currState2, inning)
				} yield pair
			} else {  // bottom half; team 2 offense
				val nextState2 = playHalfInning(currState2)
				inning += 0.5
				for {
					state2 <- nextState2
					pair <- playInnings(currState1, state2, inning)
				} yield pair
			}
		} else if (inning == 8.5) {  // top 9th is completed
			if (currState1.runs < currState2.runs) {  // team 2 has won
				Constant(currState1, currState2)
			} else {  // team 2 hasn't won, so plays offense for bot 9th
				val nextState2 = playHalfInning(currState2)
				inning += 0.5
				for {
					state2 <- nextState2
					pair <- playInnings(currState1, state2, inning)
				} yield pair
			}
		} else {  // bot 9th is completed
			// println("\n--- GAME OVER ---\n")
			Constant(currState1, currState2)
		}
		// else {  // bot 9th or more is completed; TODO: end the game as soon as the home team is up
		// 	if (currState1.runs == currState2.runs) {
		// 		val nextState1 = playHalfInning(currState1)
		// 		val nextState2 = playHalfInning(currState2)
		// 		inning += 1
		// 		for {
		// 			state1 <- nextState1
		// 			state2 <- nextState2
		// 			pair <- playInnings(state1, state2, inning)
		// 		} yield pair
		// 	} else {
		// 		Constant(currState1, currState2)
		// 	}
		// }
	}

	/*	This function plays out an half inning until there are 3 outs. As it
	 *	runs, it updates the runs, bases, players on the bases, and the
	 *	players' total statistics. 
	 *  When it finishes, it resets the outs to 0, bases to "000", and base1,
	 *	base2, and base3 to -1
	 *	3 outs x 8 states x 7 plays = 168 scenarios
	 */
	def playHalfInning(currState: scorecard): Element[scorecard] = {

		// copy variables to make the names shorter
		var batters = currState.batters
		var oppPitcher = currState.oppPitcher
		var runs = currState.runs
		var outs = currState.outs
		var bases = currState.bases
		var base1 = currState.base1
		var base2 = currState.base2
		var base3 = currState.base3
		var currBatterOrder = currState.currBatterOrder

		// if (outs >= 3) {
		// 	Constant(currState)  // home team did not score; game is a draw
		// }

		var currPlay = playAtBat(currState)

		val pFirstToThird = 2455.0 / 8879.0   // on a single; otherwise, first to second
		val pSecondToHome = 2931.0 / 5104.0   // on a single; otherwise, second to third
		val pFirstToHome  = 1051.0 / 2425.0   // on a double; otherwise, first to third

		def getNewBatterOrder(currBatterOrder: Int): Int = {
			if (currBatterOrder >= 8) 0
			else currBatterOrder + 1
		}

		for {
			play <- currPlay
			newBases <- if (play == "1B") {
							// println("batter " + currBatterOrder + " -> Play: " + play)
							Select(pSecondToHome -> "110",
							   	   pSecondToHome * pFirstToThird -> "101",
							       1 - (pSecondToHome * pFirstToThird) -> "111")
					    } else if (play == "2B") {
					    	// println("batter " + currBatterOrder + " -> Play: " + play)
					    	Select(pFirstToHome -> "010", 1 - pFirstToHome -> "011")
					    } else if (play == "3B") {
					    	// println("batter " + currBatterOrder + " -> Play: " + play)
					    	Constant("001")
					    } else if (play == "HR") {
							// println("batter " + currBatterOrder + " -> Play: " + play)
					    	Constant("000")
					    } else if (play == "TW") {
					    	// println("batter " + currBatterOrder + " -> Play: " + play)
					    	Constant("111")
					    } else if (play == "SO") {
					    	// println("batter " + currBatterOrder + " -> Play: " + play)
					    	Constant("111")
					    } else { // (play == "BO") 
					    	// println("batter " + currBatterOrder + " -> Play: " + play)
					    	Select(0.3 -> "110", 0.1 -> "101", 0.6 -> "111")
					    }

			recurse <- if (play == "SO") {
							val newBatterOrder = getNewBatterOrder(currBatterOrder)
							val newPA = batters(currBatterOrder).tPA + 1
							val newOuts = outs + 1
							val newSO = batters(currBatterOrder).tSO + 1
							val newCurrBatter = batters(currBatterOrder).copy(tPA = newPA, tSO = newSO)
							val newBatters = batters.updated(currBatterOrder, newCurrBatter)
							val newState = currState.copy(outs = newOuts, batters = newBatters, currBatterOrder = newBatterOrder)
							if (newOuts >= 3) {
								Constant(newState)
							} else {
								playHalfInning(newState)
							}
					   } else if ((play == "BO") && (newBases == "111")) {
					   		val newBatterOrder = getNewBatterOrder(currBatterOrder)
							val newPA = batters(currBatterOrder).tPA + 1
					   		val newOuts = outs + 1
					   		val newBO = batters(currBatterOrder).tBO + 1
							val newCurrBatter = batters(currBatterOrder).copy(tPA = newPA, tBO = newBO)
							val newBatters = batters.updated(currBatterOrder, newCurrBatter)
							val newState = currState.copy(outs = newOuts, batters = newBatters, currBatterOrder = newBatterOrder)
							playHalfInning(newState)
					   } else {  // not going to fall in this case
					   		Constant(currState)
					   }
		} yield {
			val newBatterOrder = getNewBatterOrder(currBatterOrder)
			val newPA = batters(currBatterOrder).tPA + 1
			if (play == "1B") {
				val newBase1 = currBatterOrder
				val new1B = batters(currBatterOrder).t1B + 1
				val newRun3rd = batters(base3).tR + 1
				val newRunner3rd = batters(base3).copy(tR = newRun3rd)
				if (newBases == "110") {
					val newRuns = runs + 2
					val newBase3 = -1
					val newBase2 = base1
					val newRun2nd = batters(base2).tR + 1
					val newRBI = batters(currBatterOrder).tRBI + 2
					val newCurrBatter = batters(currBatterOrder).copy(tPA = newPA, t1B = new1B, tRBI = newRBI)
					val newRunner2nd = batters(base2).copy(tR = newRun2nd)
					val newBatters = batters.updated(currBatterOrder, newCurrBatter)
					val newNewBatters = newBatters.updated(base2, newRunner2nd)
					val newNewNewBatters = newNewBatters.updated(base3, newRunner3rd)
					currState.copy(bases = newBases, runs = newRuns, base1 = newBase1, base2 = newBase2, base3 = newBase3, batters = newNewNewBatters, currBatterOrder = newBatterOrder)
				} else if (newBases == "101") {
					val newRuns = runs + 2
					val newBase3 = base1
					val newBase2 = -1
					val newRun2nd = batters(base2).tR + 1
					val newRBI = batters(currBatterOrder).tRBI + 2
					val newCurrBatter = batters(currBatterOrder).copy(tPA = newPA, t1B = new1B, tRBI = newRBI)
					val newRunner2nd = batters(base2).copy(tR = newRun2nd)
					val newBatters = batters.updated(currBatterOrder, newCurrBatter)
					val newNewBatters = newBatters.updated(base3, newRunner3rd)
					val newNewNewBatters = newNewBatters.updated(base2, newRunner2nd)
					currState.copy(bases = newBases, runs = newRuns, base1 = newBase1, base2 = newBase2, base3 = newBase3, batters = newNewNewBatters, currBatterOrder = newBatterOrder)
				} else {  // (newBases == "111")
					val newRuns = runs + 1
					val newBase3 = base2
					val newBase2 = base1
					val newRBI = batters(currBatterOrder).tRBI + 1
					val newCurrBatter = batters(currBatterOrder).copy(tPA = newPA, t1B = new1B, tRBI = newRBI)
					val newBatters = batters.updated(currBatterOrder, newCurrBatter)
					val newNewBatters = newBatters.updated(base3, newRunner3rd)
					currState.copy(runs = newRuns, base1 = newBase1, base2 = newBase2, base3 = newBase3, batters = newNewBatters, currBatterOrder = newBatterOrder)
				}
			} else if (play == "2B") {
				val newBase2 = currBatterOrder
				val new2B = batters(currBatterOrder).t2B + 1
				val newBase1 = -1
				val newRun3rd = batters(base3).tR + 1
				val newRun2nd = batters(base2).tR + 1
				val newRunner3rd = batters(base3).copy(tR = newRun3rd)
				val newRunner2nd = batters(base2).copy(tR = newRun2nd)
				if (newBases == "010") {
					val newRuns = runs + 3
					val newBase3 = -1
					val newRun1st = batters(base1).tR + 1
					val newRunner1st = batters(base1).copy(tR = newRun1st)
					val newRBI = batters(currBatterOrder).tRBI + 3
					val newCurrBatter = batters(currBatterOrder).copy(tPA = newPA, t2B = new2B, tRBI = newRBI)
					val newBatters = batters.updated(currBatterOrder, newCurrBatter)
					val newNewBatters = newBatters.updated(base3, newRunner3rd)
					val newNewNewBatters = newNewBatters.updated(base2, newRunner2nd)
					val newNewNewNewBatters = newNewNewBatters.updated(base1, newRunner1st)
					currState.copy(bases = newBases, runs = newRuns, base1 = newBase1, base2 = newBase2, base3 = newBase3, batters = newNewNewNewBatters, currBatterOrder = newBatterOrder)
				} else {  // (newBases == "011")
					val newRuns = runs + 2
					val newBase3 = base1
					val newRBI = batters(currBatterOrder).tRBI + 2
					val newCurrBatter = batters(currBatterOrder).copy(tPA = newPA, t2B = new2B, tRBI = newRBI)
					val newBatters = batters.updated(currBatterOrder, newCurrBatter)
					val newNewBatters = newBatters.updated(base3, newRunner3rd)
					val newNewNewBatters = newNewBatters.updated(base2, newRunner2nd)
					currState.copy(bases = newBases, runs = newRuns, base1 = newBase1, base2 = newBase2, base3 = newBase3, batters = newNewNewBatters, currBatterOrder = newBatterOrder)
				}
			} else if (play == "3B") {
				val newRuns = runs + 3
				val newBase3 = currBatterOrder
				val newBase2 = -1
				val newBase1 = -1
				val newRun3rd = batters(base3).tR + 1
				val newRun2nd = batters(base2).tR + 1
				val newRun1st = batters(base1).tR + 1
				val newRunner3rd = batters(base3).copy(tR = newRun3rd)
				val newRunner2nd = batters(base2).copy(tR = newRun2nd)
				val newRunner1st = batters(base1).copy(tR = newRun1st)
				val new3B = batters(currBatterOrder).t3B + 1
				val newRBI = batters(currBatterOrder).tRBI + 3
				val newCurrBatter = batters(currBatterOrder).copy(tPA = newPA, t3B = new3B, tRBI = newRBI)
				val newBatters = batters.updated(currBatterOrder, newCurrBatter)
				val newNewBatters = newBatters.updated(base3, newRunner3rd)
				val newNewNewBatters = newNewBatters.updated(base2, newRunner2nd)
				val newNewNewNewBatters = newNewNewBatters.updated(base1, newRunner1st)
				currState.copy(bases = newBases, runs = newRuns, base1 = newBase1, base2 = newBase2, base3 = newBase3, batters = newNewNewNewBatters, currBatterOrder = newBatterOrder)
			} else if (play == "HR") {
				val newRuns = runs + 4
				val newBase3 = -1
				val newBase2 = -1
				val newBase1 = -1
				val newRun3rd = batters(base3).tR + 1
				val newRun2nd = batters(base2).tR + 1
				val newRun1st = batters(base1).tR + 1
				val newRunner3rd = batters(base3).copy(tR = newRun3rd)
				val newRunner2nd = batters(base2).copy(tR = newRun2nd)
				val newRunner1st = batters(base1).copy(tR = newRun1st)
				val newHR = batters(currBatterOrder).tHR + 1
				val newRBI = batters(currBatterOrder).tRBI + 4
				val newR = batters(currBatterOrder).tR + 1
				val newCurrBatter = batters(currBatterOrder).copy(tPA = newPA, tHR = newHR, tRBI = newRBI, tR = newR)
				val newBatters = batters.updated(currBatterOrder, newCurrBatter)
				val newNewBatters = newBatters.updated(base3, newRunner3rd)
				val newNewNewBatters = newNewBatters.updated(base2, newRunner2nd)
				val newNewNewNewBatters = newNewNewBatters.updated(base1, newRunner1st)
				currState.copy(bases = newBases, runs = newRuns, base1 = newBase1, base2 = newBase2, base3 = newBase3, batters = newNewNewNewBatters, currBatterOrder = newBatterOrder)
			} else if (play == "TW") {
				val newRuns = runs + 1
				val newBase3 = base2
				val newBase2 = base1
				val newBase1 = currBatterOrder
				val newRun3rd = batters(base3).tR + 1
				val newRunner3rd = batters(base3).copy(tR = newRun3rd)
				val newTW = batters(currBatterOrder).tTW + 1
				val newRBI = batters(currBatterOrder).tRBI + 1
				val newCurrBatter = batters(currBatterOrder).copy(tPA = newPA, tTW = newTW, tRBI = newRBI)
				val newBatters = batters.updated(currBatterOrder, newCurrBatter)
				val newNewBatters = newBatters.updated(base3, newRunner3rd)
				currState.copy(runs = newRuns, base1 = newBase1, base2 = newBase2, base3 = newBase3, batters = newNewBatters, currBatterOrder = newBatterOrder)
			} else if (play == "SO") {
				// update currState and recurse
				recurse
			} else {  // (play == "BO")
				val newOuts = outs + 1
				val newBO = batters(currBatterOrder).tBO + 1
				if (newOuts >= 3) {
					val newCurrBatter = batters(currBatterOrder).copy(tPA = newPA, tBO = newBO)
					val newBatters = batters.updated(currBatterOrder, newCurrBatter)
					currState.copy(bases = newBases, batters = newBatters, currBatterOrder = newBatterOrder)
				} else {
					if (newBases == "110") {
						val newRuns = runs + 1
						val newBase3 = -1
						val newRun3rd = batters(base3).tR + 1
						val newRunner3rd = batters(base3).copy(tR = newRun3rd)
						val newRBI = batters(currBatterOrder).tRBI + 1
						val newCurrBatter = batters(currBatterOrder).copy(tPA = newPA, tBO = newBO, tRBI = newRBI)
						val newBatters = batters.updated(currBatterOrder, newCurrBatter)
						val newNewBatters = newBatters.updated(base3, newRunner3rd)
						currState.copy(bases = newBases, runs = newRuns, base3 = newBase3, batters = newNewBatters, currBatterOrder = newBatterOrder)
					} else if (newBases == "101") {
						val newRuns = runs + 1
						val newBase3 = base2
						val newBase2 = -1
						val newRun3rd = batters(base3).tR + 1
						val newRunner3rd = batters(base3).copy(tR = newRun3rd)
						val newRBI = batters(currBatterOrder).tRBI + 1
						val newCurrBatter = batters(currBatterOrder).copy(tPA = newPA, tBO = newBO, tRBI = newRBI)
						val newBatters = batters.updated(currBatterOrder, newCurrBatter)
						val newNewBatters = newBatters.updated(base3, newRunner3rd)
						currState.copy(bases = newBases, runs = newRuns, base3 = newBase3, base2 = newBase2, batters = newNewBatters, currBatterOrder = newBatterOrder)
					} else {  // (newBases == "111")
						// update currState and recurse
						recurse
					}
				}
			}
		} // end of yield
	}

	/*	This function considers the batter, pitcher, and league probabilities
	 * 	and uses the odds ratio method to calculate the probabilities for 
	 *	individual events occurring during an at-bat.
	 */
	def playAtBat(currState: scorecard): Element[String] = {
		// copy variables to make the names shorter
		var batters = currState.batters
		var oppPitcher = currState.oppPitcher
		var currBatterOrder = currState.currBatterOrder

		val oddsBO = (batters(currBatterOrder).pBO / (1 - batters(currBatterOrder).pBO)) * 
					 (oppPitcher.pBO / (1 - oppPitcher.pBO)) / 
					 (league.pBO / (1 - league.pBO))
		val pBO = (oddsBO / (1 + oddsBO))

		val odds1B = (batters(currBatterOrder).p1B / (1 - batters(currBatterOrder).p1B)) * 
					 (oppPitcher.p1B / (1 - oppPitcher.p1B)) / 
					 (league.p1B / (1 - league.p1B))
		val p1B = (odds1B / (1 + odds1B))

		val odds2B = (batters(currBatterOrder).p2B / (1 - batters(currBatterOrder).p2B)) * 
					 (oppPitcher.p2B / (1 - oppPitcher.p2B)) / 
					 (league.p2B / (1 - league.p2B))
		val p2B = (odds2B / (1 + odds2B))

		val odds3B = (batters(currBatterOrder).p3B / (1 - batters(currBatterOrder).p3B)) * 
					 (oppPitcher.p3B / (1 - oppPitcher.p3B)) / 
					 (league.p3B / (1 - league.p3B))
		val p3B = (odds3B / (1 + odds3B)) 

		val oddsHR = (batters(currBatterOrder).pHR / (1 - batters(currBatterOrder).pHR)) *
					 (oppPitcher.pHR / (1 - oppPitcher.pHR)) /
					 (league.pHR / (1 - league.pHR))
		val pHR = (oddsHR / (1 + oddsHR)) 

		val oddsTW = (batters(currBatterOrder).pTW / (1 - batters(currBatterOrder).pTW)) *
					 (oppPitcher.pTW / (1 - oppPitcher.pTW)) /
					 (league.pTW / (1 - league.pTW))
		val pTW = (oddsTW / (1 + oddsTW))

		val oddsSO = (batters(currBatterOrder).pSO / (1 - batters(currBatterOrder).pSO)) *
					 (oppPitcher.pSO / (1 - oppPitcher.pSO)) /
					 (league.pSO / (1 - league.pSO))
		val pSO = (oddsSO / (1 + oddsSO))

		val currPlay = Select(p1B -> "1B", p2B -> "2B", p3B -> "3B", pHR -> "HR", pTW -> "TW", pSO -> "SO", pBO -> "BO")
		currPlay
	}

	/*	This function prints out the box score after a game is completed
	 */
	def printBoxScore(scorecard1: scorecard, scorecard2: scorecard) {
		// helper(scorecard1, 1)
		helper(scorecard2, 2)
		def helper(card: scorecard, n: Int) {
			var batters = card.batters
			println()
			println("-----------|----|----|---|----|----|----|----|-----|----|----|")
			println("|  team " + n + "  | PA | BO | R | 1B | 2B | 3B | HR | RBI | TW | SO |")
			println("|----------|----|----|---|----|----|----|----|-----|----|----|")
			for (i <- 0 until 9) {
				println("| batter " + i + " | " + batters(i).tPA + "  | " 
						+ batters(i).tBO + "  | " + batters(i).tR + " | " 
						+ batters(i).t1B + "  | " + batters(i).t2B + "  | " 
						+ batters(i).t3B + "  | " + batters(i).tHR + "  |  " 
						+ batters(i).tRBI + "  | " + batters(i).tTW + "  | " 
						+ batters(i).tSO + "  |")
			}
			println("|----------|----|----|---|----|----|----|----|-----|----|----|")
			println()
		}
	}

	def runAlgorithms(triple: Element[(scorecard, scorecard, Boolean)]) {

		val team2Wins = triple._3
		println("\n############################################################")
		println("Running Importance Sampling algorithm ...")
		println("\nQ1 -> Win probability of team 2 winning, when the game is tied, "
				+ "bases are loaded with 1 out, and the No.4 batter is up:")
		// for {
		// 	results <- triple
		// } yield {
		// 	printBoxScore(results._1, results._2)
		// }
		val imp = Importance(100000, team2Wins)
		imp.start()
		println("\n   " + imp.probability(team2Wins, true))
		imp.kill()

		// team2Wins.observe(true)

  		val b3RBI = for {
	  					card <- triple._2
	  				} yield {
	  					if (card.batters(3).tRBI > 0) true
	  					else false
	  				}
	  	val b4RBI = for {
	  					card <- triple._2
	  				} yield {
	  					if (card.batters(4).tRBI > 0) true
	  					else false
	  				}
  		val b5RBI = for {
  						card <- triple._2
  					} yield {
  						if (card.batters(5).tRBI > 0) true
  						else false
  					}

  		val b3tRBI = for {
	  					card <- triple._2
	  				} yield {
	  					card.batters(3).tRBI.toDouble
	  				}
	  	val b4tRBI = for {
	  					card <- triple._2
	  				} yield {
	  					card.batters(4).tRBI.toDouble
	  				}
  		val b5tRBI = for {
  						card <- triple._2
  					} yield {
						card.batters(5).tRBI.toDouble
  					}

  		println("\nQ2 -> Given that team 2 won the game, ")
	  	val imp2 = Importance(100000, b3RBI, b4RBI, b5RBI, b3tRBI, b4tRBI, b5tRBI)
	  	imp2.start()
	  	println("\n   probability that batter(3) "
	  			+ "got the game-winning RBI: " + imp2.probability(b3RBI, true))
	  	println("\n   probability that batter(4) "
	  			+ "got the game-winning RBI: " + imp2.probability(b4RBI, true))
		println("\n   probability that batter(5) "
	  			+ "got the game-winning RBI: " + imp2.probability(b5RBI, true))
		println("\n   average number of RBIs for batter(3): "+ imp2.mean(b3tRBI))
		println("\n   average number of RBIs for batter(4): "+ imp2.mean(b4tRBI))
		println("\n   average number of RBIs for batter(5): "+ imp2.mean(b5tRBI))
	  	println("\n############################################################\n")
	  	imp2.kill()

	  	team2Wins.unobserve()

  //       println("############################################################")
		// val mpeMH = MetropolisHastingsAnnealer(100000, ProposalScheme.default, Schedule.default(1.0))
		// println("MPE Metropolis Hastings running ...")
  //       mpeMH.start()
  //       println("Given that team 2 has won the game, what's the most probable value of: ")
  //       println("Batter3's RBI: " + mpeMH.mostLikelyValue(b3tRBI))
  //       mpeMH.kill()

  //      	team2Wins.unobserve()
	}

	def main(args: Array[String]) {
		checkArgsLength(args)
		createDatabases()
		checkValidNames(args)
		initializeProbs()
		val triple = playGame(3)
		runAlgorithms(triple)
	}
}


