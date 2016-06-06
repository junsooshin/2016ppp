import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.algorithm.factored.VariableElimination

object tennisExercise {

	val probP1Serve = 0.55
	val probP2Serve = 0.50

	val Match: Element[Boolean] = mkMatch(0, 0)  // starts the match

	def mkMatch(p1Sets: Int, p2Sets: Int): Element[Boolean] = {
		// if ((p1Sets == 0) && (p2Sets == 0)) println("NEW MATCH")
		val setNum = p1Sets + p2Sets + 1
		if (p1Sets == 3) {
			// println("P1 Wins " + p1Sets + "-" + p2Sets)
			// println("##############################")
			Constant(true)}
		else if (p2Sets == 3) {
			// println("P2 Wins " + p1Sets + "-" + p2Sets)
			// println("##############################")
			Constant(false)}
		else for {
			server <- Flip(0.5)  // this might not alternate server on a new set
			setResult <- mkSet(0, 0, setNum, server)  // calls mkSet() and gets the result when the set is over
			nextSet <- if (setResult) mkMatch(p1Sets + 1, p2Sets)
			           else mkMatch(p1Sets, p2Sets + 1)
		} yield nextSet
	}

	def mkSet(p1Games: Int, p2Games: Int, setNum: Int, server: Boolean): Element[Boolean] = {
		if ((setNum != 5) && (p1Games == 6) && (p2Games == 6)) tiebreak(0, 0, setNum, server)
		else if ((p1Games >= 6) && ((p1Games - p2Games) >= 2)) {
			// println("Set " + setNum + " Completed " + p1Games + ":" + p2Games)
			Constant(true)}
		else if ((p2Games >= 6) && ((p2Games - p1Games) >= 2)) {
			// println("Set " + setNum + " Completed " + p1Games + ":" + p2Games)
			Constant(false)}
		else for {
				  gameResult <- mkGame(0, 0, server)
				  nextGame <- if (gameResult) mkSet(p1Games + 1, p2Games, setNum, !server)
				              else mkSet(p1Games, p2Games + 1, setNum, !server)
			} yield nextGame			
	}

	/* Objective: A player needs to win 7 points first and be up by 2 points
	 * Rule: The player, who would have served on the game after 6-6, is the first server.
	 *		 The first server only serves the first point, and then for the rest of the tiebreak,
	 *       the players alternate serves for 2 points
	 */
	def tiebreak(p1Points: Int, p2Points: Int, setNum: Int, server: Boolean): Element[Boolean] = {
		val nextServer = if (((p1Points + p2Points) % 2) == 1) !server
						 else server
		if ((p1Points >= 7) && ((p1Points - p2Points) >= 2)) {
			// println("Set " + setNum + " Completed 7:6 (" + p1Points + ":" + p2Points + ")")
			Constant(true)}  
		else if ((p2Points >= 7) && ((p2Points - p1Points) >= 2)) {
			// println("Set " + setNum + " Completed 6:7 (" + p1Points + ":" + p2Points + ")")
			Constant(false)}
		else for {
				pointResult <- Flip( if (nextServer) probP1Serve else probP2Serve )
				nextPoint <- if (pointResult) tiebreak(p1Points + 1, p2Points, setNum, nextServer)
						     else tiebreak(p1Points, p2Points + 1, setNum, nextServer)
			} yield nextPoint
	}

	def mkGame(p1Points: Int, p2Points: Int, server: Boolean): Element[Boolean] = {
		if ((p1Points >= 4) && ((p1Points - p2Points) >= 2)) {
			// println("  " + p1Points + ":" + p2Points)
			Constant(true)}
		else if ((p2Points >= 4) && ((p2Points - p1Points) >= 2)) {
			// println("  " + p1Points + ":" + p2Points)
			Constant(false)}
		else for {
			pointResult <- Flip( if (server) probP1Serve else probP2Serve )
			nextPoint <- if (pointResult) mkGame(p1Points + 1, p2Points, server)
				         else mkGame(p1Points, p2Points + 1, server)
		} yield nextPoint
	}

	def main(args: Array[String]) {
		println("Win Probability of P1: " + Importance.probability(Match, true))
	}
}














