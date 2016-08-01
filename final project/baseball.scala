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
import util.control.Breaks._  // for "breakable" and break" method
import com.cra.figaro.algorithm.sampling.Importance

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

		var p1B: Double = 0  // probabilities to simulate a play
		var p2B: Double = 0
		var p3B: Double = 0
		var pHR: Double = 0
		var pTW: Double = 0
		var pSO: Double = 0
		var pBO: Double = 0

		var t1B: Int = 0  // stats for box score
		var t2B: Int = 0
		var t3B: Int = 0
		var tHR: Int = 0
		var tTW: Int = 0
		var tSO: Int = 0
		var tBO: Int = 0

		var tPA: Int = 0
		var tR: Int = 0
		var tRBI: Int = 0
	}

	class scorecard {
		var batters = Array.fill(9)(new player)
		var oppPitcher = new player
		var runs = 0
		var outs = 0
		var bases: Element[String] = Constant("000")  // bases occupied; 0 is empty, 1 is full; 1st 2nd 3rd
		var base1 = -1  // indicates who is on 1st base by batter order
		var base2 = -1
		var base3 = -1
		var currBatterOrder = 0
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

	/*	This function creates two scorecards and calls playInnings.
	 * 	It gets back a pair of scorecards when the game finishes.
	 */
	def playGame(): Element[Boolean] = {
		val initState1 = new scorecard
		val initState2 = new scorecard
		initState1.batters = batters1
		initState2.batters = batters2
		initState1.oppPitcher = pitcher2
		initState2.oppPitcher = pitcher1
		initState1.runs = 1
		val pair = playInnings(initState1, initState2, 0.0)
		if (pair._1.runs > pair._2.runs) Constant(true)
		else Constant(false)
	}

	/*	This function calls playHalfInning with an appropriate scorecard,
	 *	depending on whether an inning is top or bottom.
	 *	It returns a pair of scorecards when the game finishes.
	 */
	def playInnings(currState1: scorecard, currState2: scorecard, inn: Double): (scorecard, scorecard) = {
		
		var inning = inn

		println("Inning: " + inning)
		println("Score: " + currState1.runs + "-" + currState2.runs)

		if (inning <= 8) {  // top 9th isn't completed yet
			if (inning - (inning.floor) == 0) {  // top half; team 1 offense
				val nextState1 = playHalfInning(currState1)
				inning += 0.5
				playInnings(nextState1, currState2, inning)
			} else {  // bottom half; team 2 offense
				val nextState2 = playHalfInning(currState2)
				inning += 0.5
				playInnings(currState1, nextState2, inning)
			}
		} else if (inning == 8.5) {  // top 9th is completed
			if (currState1.runs < currState2.runs)  {  // team 2 has won
				val pair = (currState1, currState2)
				pair
			} else {  // team 2 hasn't won, so plays offense for bot 9th
				val nextState2 = playHalfInning(currState2)
				inning += 0.5
				playInnings(currState1, nextState2, inning)
			}
		} else {  // bot 9th or more is completed
			if (currState1.runs == currState2.runs) {
				val nextState1 = playHalfInning(currState1)
				val nextState2 = playHalfInning(currState2)
				inning += 1
				playInnings(nextState1, nextState2, inning)
			} else {
				val pair = (currState1, currState2)
				pair
			}
		}
	}

	/*
	class scorecard {
		var batters = Array.fill(9)(new player)
		var oppPitcher = new player
		var runs = 0
		var outs = 0
		var bases = "000"
		var base1 = -1
		var base2 = -1
		var base3 = -1
		var currBatterOrder = 0
	}
	*/

	/*	This function plays out an half inning until there are 3 outs. As it
	 *	runs, it updates the runs, bases, players on the bases, and the
	 *	players' total statistics. 
	 *  When it finishes, it resets the outs to 0, bases to "000", and base1,
	 *	base2, and base3 to -1
	 *	3 outs x 8 states x 7 plays = 168 scenarios
	 */
	def playHalfInning(currState: scorecard): scorecard = {

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
		var currPlay = playAtBat(currState)

		val pFirstToThird = 2455.0 / 8879.0   // on a single; otherwise, first to second
		val pSecondToHome = 2931.0 / 5104.0   // on a single; otherwise, second to third
		val pFirstToHome  = 1051.0 / 2425.0   // on a double; otherwise, first to third

		currPlay = "2B"
		
		batters(currBatterOrder).tPA += 1
		println("Play: " + currPlay)
		println("OUTS: " + outs)

		if (currPlay == "2B") {
			println("YES")
		}

		if ((outs == 0) || (outs == 1) || (outs == 2)) {
			if (bases == Constant("000")) {
				if (currPlay == Constant("1B")) {
					bases = Constant("100")
					base1 = currBatterOrder
					batters(currBatterOrder).t1B += 1
				} else if (currPlay == Constant("2B")) {
					bases = Constant("010")
					base2 = currBatterOrder
					batters(currBatterOrder).t2B += 1
				} else if (currPlay == Constant("3B")) {
					bases = Constant("001")
					base3 = currBatterOrder
					batters(currBatterOrder).t3B += 1
				} else if (currPlay == Constant("HR")) {
					bases = Constant("000")
					runs += 1
					batters(currBatterOrder).tHR += 1
					batters(currBatterOrder).tR += 1
					batters(currBatterOrder).tRBI += 1
				} else if (currPlay == Constant("TW")) {
					bases = Constant("100")
					base1 = currBatterOrder
					batters(currBatterOrder).tTW += 1
				} else if (currPlay == Constant("SO")) {
					outs += 1
					batters(currBatterOrder).tSO += 1
				}
			} else if (bases == Constant("100")) {
				if (currPlay == Constant("1B")) {
					bases = Select(pFirstToThird -> "101", 1 - pFirstToThird -> "110")
					if (bases == Constant("101")) {
						base3 = base1
					} else {
						base2 = base1
					}
					base1 = currBatterOrder
					batters(currBatterOrder).t1B += 1
				} else if (currPlay == Constant("2B")) {
					bases = Select(pFirstToHome -> "010", 1 - pFirstToHome -> "011")
					if (bases == Constant("010")) {
						batters(base1).tR += 1
						batters(currBatterOrder).tRBI += 1
					} else {
						base3 = base1
					}
					base1 = -1
					base2 = currBatterOrder
					batters(currBatterOrder).t2B += 1
				} else if (currPlay == Constant("3B")) {
					bases = Constant("001")
					batters(base1).tR += 1
					runs += 1
					base1 = -1
					base3 = currBatterOrder
					batters(currBatterOrder).t3B += 1
					batters(currBatterOrder).tRBI += 1
				} else if (currPlay == Constant("HR")) {
					bases = Constant("000")
					batters(base1).tR += 1
					batters(currBatterOrder).tHR += 1
					batters(currBatterOrder).tR += 1
					batters(currBatterOrder).tRBI += 2
					runs += 2
					base1 = -1
				} else if (currPlay == Constant("TW")) {
					bases = Constant("110")
					base2 = base1
					base1 = currBatterOrder
					batters(currBatterOrder).tTW += 1
				} else if (currPlay == Constant("SO")) {
					outs += 1
					batters(currBatterOrder).tSO += 1
				}
			} else if (bases == Constant("010")) {
				if (currPlay == Constant("1B")) {
					bases = Select(pSecondToHome -> "100", 1 - pSecondToHome -> "101")
					if (bases == Constant("100")) {
						batters(base2).tR += 1
						runs += 1
						batters(currBatterOrder).t1B += 1
						batters(currBatterOrder).tRBI += 1
						base2 = -1
					} else {
						base3 = base2
						base2 = -1
					}
					base1 = currBatterOrder
				} else if (currPlay == Constant("2B")) {
					bases == Constant("010")
					batters(base2).tR += 1
					runs += 1
					batters(currBatterOrder).t2B += 1
					batters(currBatterOrder).tRBI += 1
					base2 = currBatterOrder
				} else if (currPlay == Constant("3B")) {
					bases == Constant("001")
					batters(base2).tR += 1
					runs += 1
					batters(currBatterOrder).t3B += 1
					batters(currBatterOrder).tRBI += 1
					base2 = -1
					base3 = currBatterOrder
				} else if (currPlay == Constant("HR")) {
					bases = Constant("000")
					batters(base2).tR += 1
					batters(currBatterOrder).tHR += 1
					batters(currBatterOrder).tR += 1
					batters(currBatterOrder).tRBI += 2
					runs += 2
					base2 = -1
				} else if (currPlay == Constant("TW")) {
					bases = Constant("110")
					batters(currBatterOrder).tTW += 1
					base1 = currBatterOrder
				} else if (currPlay == Constant("SO")) {
					batters(currBatterOrder).tSO += 1
					outs += 1
				}
			} else if (bases == Constant("001")) {
				if (currPlay == Constant("1B")) {
					bases = Constant("100")
					batters(base3).tR += 1
					runs += 1
					base3 = -1
					batters(currBatterOrder).t1B += 1
					batters(currBatterOrder).tRBI += 1
					base1 = currBatterOrder
				} else if (currPlay == Constant("2B")) {
					bases = Constant("010")
					batters(base3).tR += 1
					runs += 1
					base3 = -1
					batters(currBatterOrder).t2B += 1
					batters(currBatterOrder).tRBI += 1
					base2 = currBatterOrder
				} else if (currPlay == Constant("3B")) {
					bases = Constant("001")
					batters(base3).tR += 1
					runs += 1
					base3 = -1
					batters(currBatterOrder).t3B += 1
					batters(currBatterOrder).tRBI += 1
					base3 = currBatterOrder
				} else if (currPlay == Constant("HR")) {
					bases = Constant("000")
					batters(base3).tR += 1
					batters(currBatterOrder).tHR += 1
					batters(currBatterOrder).tR += 1
					batters(currBatterOrder).tRBI += 2
					runs += 2
					base3 = -1
				} else if (currPlay == Constant("TW")) {
					bases = Constant("101")
					batters(currBatterOrder).tTW += 1
					base1 = currBatterOrder
				} else if (currPlay == Constant("SO")) {
					batters(currBatterOrder).tSO += 1
					outs += 1
				}
			} else if (bases == Constant("110")) {
				if (currPlay == Constant("1B")) {
					bases = Select(pSecondToHome -> "110", 
								   pSecondToHome * pFirstToThird -> "101", 
								   1 - (pSecondToHome + (pSecondToHome * pFirstToThird)) -> "111")
					if (bases == Constant("110")) {
						batters(base2).tR += 1
						batters(currBatterOrder).tRBI += 1
						base2 = base1
						runs += 1
					} else if (bases == Constant("101")) {
						batters(base2).tR += 1
						batters(currBatterOrder).tRBI += 1
						base2 = -1
						base3 = base1
					} else {
						base3 = base2
						base2 = base1
					}
					batters(currBatterOrder).t1B += 1
					base1 = currBatterOrder
				} else if (currPlay == Constant("2B")) {
					bases = Select(pFirstToHome -> "010", 1 - pFirstToHome -> "011")
					if (bases == Constant("010")) {
						batters(base1).tR += 1
						batters(currBatterOrder).tRBI += 2
						runs += 2
					} else {
						base3 = base1
						batters(currBatterOrder).tRBI += 1
						runs += 1
					}
					batters(base2).tR += 1
					batters(currBatterOrder).t2B += 1
					base2 = currBatterOrder
					base1 = -1
				} else if (currPlay == Constant("3B")) {
					bases = Constant("001")
					batters(base2).tR += 1
					batters(base1).tR += 1
					runs += 2
					batters(currBatterOrder).tRBI += 2
					batters(currBatterOrder).t3B += 1
					base3 = currBatterOrder
					base2 = -1
					base1 = -1
				} else if (currPlay == Constant("HR")) {
					bases = Constant("000")
					batters(base2).tR += 1
					batters(base1).tR += 1
					batters(currBatterOrder).tHR += 1
					batters(currBatterOrder).tRBI += 3
					batters(currBatterOrder).tR += 1
					runs += 3
					base1 = -1
					base2 = -1
				} else if (currPlay == Constant("TW")) {
					bases = Constant("111")
					base3 = base2
					base2 = base1
					base1 = currBatterOrder
					batters(currBatterOrder).tTW += 1
				} else if (currPlay == Constant("SO")) {
					batters(currBatterOrder).tSO += 1
					outs += 1
				}
			} else if (bases == Constant("101")) {
				if (currPlay == Constant("1B")) {
					bases = Select(pFirstToThird -> "101", 1 - pFirstToThird -> "110")
					if (bases == Constant("101")) {
						base3 = base1
					} else {
						base2 = base1
					}
					batters(base3).tR += 1
					batters(currBatterOrder).t1B += 1
					batters(currBatterOrder).tRBI += 1
					runs += 1
					base1 = currBatterOrder
				} else if (currPlay == Constant("2B")) {
					bases = Select(pFirstToHome -> "010", 1 - pFirstToHome -> "011")
					if (bases == Constant("010")) {
						batters(base1).tR += 1
						base3 = -1
						runs += 2
						batters(currBatterOrder).tRBI += 2
					} else {
						base3 = base1
						runs += 1
						batters(currBatterOrder).tRBI += 1
					}
					batters(base3).tR += 1
					batters(currBatterOrder).t2B += 1
					base2 = currBatterOrder
					base1 = -1
				} else if (currPlay == Constant("3B")) {
					bases = Constant("001")
					batters(base3).tR += 1
					batters(base1).tR += 1
					batters(currBatterOrder).t3B += 1
					batters(currBatterOrder).tRBI += 2
					runs += 2
					base3 = currBatterOrder
					base1 = -1
					base2 = -1
				} else if (currPlay == Constant("HR")) {
					bases = Constant("000")
					batters(base3).tR += 1
					batters(base1).tR += 1
					batters(currBatterOrder).tHR += 1
					batters(currBatterOrder).tRBI += 3
					batters(currBatterOrder).tR += 1
					runs += 1
					base3 = -1
					base1 = -1
				} else if (currPlay == Constant("TW")) {
					bases = Constant("101")
					batters(currBatterOrder).tTW += 1
					base2 = base1
					base1 = currBatterOrder
				} else if (currPlay == Constant("SO")) {
					batters(currBatterOrder).tSO += 1
					outs += 1
				}
			} else if (bases == Constant("011")) {
				if (currPlay == Constant("1B")) {
					bases = Select(pSecondToHome -> "100", 1 - pSecondToHome -> "101")
					if (bases == Constant("100")) {
						batters(base2).tR += 1
						base3 = -1
						batters(currBatterOrder).tRBI += 2
						runs += 2
					} else {
						base3 = base2
						batters(currBatterOrder).tRBI += 1
						runs += 1
					}
					batters(base3).tR += 1
					base2 = -1
					batters(currBatterOrder).t1B += 1
					base1 = currBatterOrder
				} else if (currPlay == Constant("2B")) {
					bases = Constant("010")
					batters(base3).tR += 1
					batters(base2).tR += 1
					base3 = -1
					base2 = currBatterOrder
					batters(currBatterOrder).t2B += 1
					batters(currBatterOrder).tRBI += 2
					runs += 2
				} else if (currPlay == Constant("3B")) {
					bases = Constant("001")
					batters(base3).tR += 1
					batters(base2).tR += 1
					base3 = currBatterOrder
					base2 = -1
					batters(currBatterOrder).t3B += 1
					batters(currBatterOrder).tRBI += 2
					runs += 2
				} else if (currPlay == Constant("HR")) {
					bases = Constant("000")
					batters(base3).tR += 1
					batters(base2).tR += 1
					batters(currBatterOrder).tR += 1
					batters(currBatterOrder).tHR += 1
					batters(currBatterOrder).tRBI += 3
					runs += 3
					base3 = -1
					base2 = -1
				} else if (currPlay == Constant("TW")) {
					bases = Constant("111")
					base1 = currBatterOrder
					batters(currBatterOrder).tTW += 1
				} else if (currPlay == Constant("SO")) {
					batters(currBatterOrder).tSO += 1
					outs += 1
				}
			} else if (bases == Constant("111")) {
				if (currPlay == Constant("1B")) {
					bases = Select(pSecondToHome -> "110",
								   pSecondToHome * pFirstToThird -> "101",
								   1 - (pSecondToHome * pFirstToThird) -> "111")
					if (bases == Constant("110")) {
						batters(base2).tR += 1
						base3 = -1
						base2 = base1
						batters(currBatterOrder).tRBI += 2
						runs += 2
					} else if (bases == Constant("101")) {
						batters(base2).tR += 1
						base3 = base1
						base2 = -1
						batters(currBatterOrder).tRBI += 2
						runs += 2
					} else {
						base3 = base2
						base2 = base1
						batters(currBatterOrder).tRBI += 1
						runs += 1
					}
					batters(base3).tR += 1
					base1 = currBatterOrder
					batters(currBatterOrder).t1B += 1
				} else if (currPlay == Constant("2B")) {
					bases = Select(pFirstToHome -> "010", 1 - pFirstToHome -> "011")
					if (bases == Constant("010")) {
						batters(base1).tR += 1
						base3 = -1
						batters(currBatterOrder).tRBI += 3
						runs += 3
					} else {
						base3 = base1
						batters(currBatterOrder).tRBI += 2
						runs += 2
					}
					batters(base3).tR += 1
					batters(base2).tR += 1
					base1 = -1
					batters(currBatterOrder).t2B += 1
					base2 = currBatterOrder
				} else if (currPlay == Constant("3B")) {
					bases = Constant("001")
					batters(base3).tR += 1
					batters(base2).tR += 1
					batters(base1).tR += 1
					batters(currBatterOrder).t3B += 1
					batters(currBatterOrder).tRBI += 3
					runs += 3
					base1 = -1
					base2 = -1
					base3 = currBatterOrder
				} else if (currPlay == Constant("HR")) {
					bases = Constant("000")
					batters(base3).tR += 1
					batters(base2).tR += 1
					batters(base1).tR += 1
					batters(currBatterOrder).tR += 1
					batters(currBatterOrder).tHR += 1
					batters(currBatterOrder).tRBI += 4
					runs += 4
					base1 = -1
					base2 = -1
					base3 = -1
				} else if (currPlay == Constant("TW")) {
					batters(base3).tR += 1
					base3 = base2
					base2 = base1
					base1 = currBatterOrder
					batters(currBatterOrder).tTW += 1
					batters(currBatterOrder).tRBI += 1
					runs += 1
				} else if (currPlay == Constant("SO")) {
					batters(currBatterOrder).tSO += 1
					outs += 1
				}
			}
		}

		if (((outs == 0) || (outs == 1)) && (currPlay == Constant("BO"))) {
			batters(currBatterOrder).tBO += 1
			outs += 1
			if (bases == Constant("000")) {
				outs += 1
			} else if (bases == Constant("100")) {
				bases = Select(0.6 -> "100", 0.4 -> "010")
				if (bases == Constant("100")) {
					base2 = -1
					base1 = currBatterOrder
				} else {
					base2 = base1
				}
			} else if (bases == Constant("010")) {
				bases = Select(0.8 -> "010", 0.2 -> "001")
				if (bases == Constant("001")) {
					base3 = base2
					base2 = -1
				}
			} else if (bases == Constant("001")) {
				bases = Select(0.5 -> "000", 0.5 -> "001")
				if (bases == Constant("000")) {
					batters(base3).tR += 1
					runs += 1
					batters(currBatterOrder).tRBI += 1
					base3 = -1
				} 
			} else if (bases == Constant("110")) {
				bases = Select(0.7 -> "110", 0.3 -> "101")
				if (bases == Constant("101")) {
					base3 = base2
					base2 = -1
				}
			} else if (bases == Constant("101")) {
				bases = Select(0.4 -> "100", 0.2 -> "011", 0.4 -> "101")
				if (bases == Constant("100")) {
					batters(base3).tR += 1
					base3 = -1
					runs += 1
					batters(currBatterOrder).tRBI += 1
				} else if (bases == Constant("011")) {
					base2 = base1
					base1 = -1
				}
			} else if (bases == Constant("011")) {
				bases = Select(0.3 -> "010", 0.2 -> "001", 0.5 -> "011")
				if (bases == Constant("010")) {
					batters(base3).tR += 1
					base3 = -1
					batters(currBatterOrder).tRBI += 1
					runs += 1
				} else if (bases == Constant("001")) {
					batters(base3).tR += 1
					base3 = base2
					base2 = -1
					batters(currBatterOrder).tRBI
					runs += 1
				}
			} else if (bases == Constant("111")) {
				bases = Select(0.4 -> "110", 0.1 -> "101", 0.5 -> "111")
				if (bases == Constant("110")) {
					batters(base3).tR += 1
					base3 = -1
					batters(currBatterOrder).tRBI += 1
					runs += 1
				} else if (bases == Constant("101")) {
					batters(base3).tR += 1
					base3 = base2
					base2 = -1
					batters(currBatterOrder).tRBI += 1
					runs += 1
				}
			}
		}

		if ((outs == 2) && (currPlay == Constant("BO"))) {
			batters(currBatterOrder).tBO += 1
			outs += 1
		}

		outs += 1

		// update the currState after a play
		currState.batters = batters
		currState.oppPitcher = oppPitcher
		currState.runs = runs
		currState.outs = outs
		currState.bases = bases
		currState.base1 = base1
		currState.base2 = base2
		currState.base3 = base3
		currState.currBatterOrder += 1
		if (currState.currBatterOrder == 9) {
			currState.currBatterOrder = 0
		}

		// recurse or return
		if (currState.outs < 3) {
			playHalfInning(currState)
		} else {
			currState.outs = 0
			currState.bases = Constant("000")
			currState.base1 = -1
			currState.base2 = -1
			currState.base3 = -1
			currState
		}
	}

	/*	This function considers the batter, pitcher, and league probabilities
	 * 	and uses the odds ratio method to calculate the probabilities for 
	 *	individual events occurring during an at-bat.
	 */
	def playAtBat(currState: scorecard): Element[String] = {

		// val sumContactB = batters1(4).p1B + batters1(4).p2B + batters1(4).p3B + batters1(4).pHR + batters1(4).pBO
		// val sumContactP = pitcher2.p1B + pitcher2.p2B + pitcher2.p3B + pitcher2.pHR + pitcher2.pBO
		// val sumContactL = league.p1B + league.p2B + league.p3B + league.pHR + league.pBO

		// val sumNotContactB = batters1(4).pTW + batters1(4).pSO
		// val sumNotContactP = pitcher2.pTW + pitcher2.pSO
		// val sumNotContactL = league.pTW + league.pSO

		// val sumNotBOB = 1 - batters1(4).pBO
		// val sumNotBOP = 1 - pitcher2.pBO
		// val sumNotBOL = 1 - league.pBO

		// val sumNot1BB = 1 - batters1(4).p1B
		// val sumNot1BP = 1 - pitcher2.p1B
		// val sumNot1BL = 1 - league.p1B

		// val sumNot2BB = 1 - batters1(4).p2B
		// val sumNot2BP = 1 - pitcher2.p2B
		// val sumNot2BL = 1 - league.p2B

		// val sumNot3BB = 1 - batters1(4).p3B
		// val sumNot3BP = 1 - pitcher2.p3B
		// val sumNot3BL = 1 - league.p3B

		// val sumNotHRB = 1 - batters1(4).pHR
		// val sumNotHRP = 1 - pitcher2.pHR
		// val sumNotHRL = 1 - league.pHR

		// val sumNotTWB = 1 - batters1(4).pTW
		// val sumNotTWP = 1 - pitcher2.pTW
		// val sumNotTWL = 1 - league.pTW

		// val sumNotSOB = 1 - batters1(4).pSO
		// val sumNotSOP = 1 - pitcher2.pSO
		// val sumNotSOL = 1 - league.pSO


		// val oddsContact = (sumContactB / (1 - sumContactB)) * (sumContactP / (1 - sumContactP)) / (sumContactL / (1 - sumContactL))
		// val pContact = oddsContact / (1 + oddsContact)

		// val oddsNotContact = (sumNotContactB / (1 - sumNotContactB)) * (sumNotContactP / (1 - sumNotContactP)) / (sumNotContactL / (1 - sumNotContactL))
		// val pNotContact = oddsNotContact / (1 + oddsNotContact)

		// val oddsNotBO = (sumNotBOB / (1 - sumNotBOB)) * (sumNotBOP / (1 - sumNotBOP)) / (sumNotBOL / (1 - sumNotBOL))
		// val pNotBO = (oddsNotBO / (1 + oddsNotBO))

		// val oddsNot1B = (sumNot1BB / (1 - sumNot1BB)) * (sumNot1BP / (1 - sumNot1BP)) / (sumNot1BL / (1 - sumNot1BL))
		// val pNot1B = (oddsNot1B / (1 + oddsNot1B))

		// val oddsNot2B = (sumNot2BB / (1 - sumNot2BB)) * (sumNot2BP / (1 - sumNot2BP)) / (sumNot2BL / (1 - sumNot2BL))
		// val pNot2B = (oddsNot2B / (1 + oddsNot2B))

		// val oddsNot3B = (sumNot3BB / (1 - sumNot3BB)) * (sumNot3BP / (1 - sumNot3BP)) / (sumNot3BL / (1 - sumNot3BL))
		// val pNot3B = (oddsNot3B / (1 + oddsNot3B))

		// val oddsNotHR = (sumNotHRB / (1 - sumNotHRB)) * (sumNotHRP / (1 - sumNotHRP)) / (sumNotHRL / (1 - sumNotHRL))
		// val pNotHR = (oddsNotHR / (1 + oddsNotHR))

		// val oddsNotTW = (sumNotTWB / (1 - sumNotTWB)) * (sumNotTWP / (1 - sumNotTWP)) / (sumNotTWL / (1 - sumNotTWL))
		// val pNotTW = (oddsNotTW / (1 + oddsNotTW))

		// val oddsNotSO = (sumNotSOB / (1 - sumNotSOB)) * (sumNotSOP / (1 - sumNotSOP)) / (sumNotSOL / (1 - sumNotSOL))
		// val pNotSO = (oddsNotSO / (1 + oddsNotSO))


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

		val currPlay = Select(p1B -> "1B", p2B -> "2B", p3B -> "3B", pHR -> "HR", pTW -> "TW", pSO -> "SO", pBO -> "BO")
		currPlay
	}


	def main(args: Array[String]) {
		checkArgsLength(args)
		createDatabases()
		checkValidNames(args)
		initializeProbs()
		val team1Wins = playGame()
		val algorithm = Importance(500, team1Wins)
		algorithm.start()
		println("Win Probability of team1: " + algorithm.probability(team1Wins, true))
		algorithm.kill()
	}
}


