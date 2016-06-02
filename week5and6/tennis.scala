import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.algorithm.factored.VariableElimination

object tennisExercise {

	val probP1Serve = 0.7
	val probP2Serve = 0.5

	val Match: Element[Boolean] = mkMatch(0, 0)  // starts the match

	def mkMatch(p1Sets: Int, p2Sets: Int): Element[Boolean] = {
		// println(p1Sets + "-" + p2Sets)

		if (p1Sets == 2) Constant(true)
		else if (p2Sets == 2) Constant(false)
		else for {
			server <- Flip(0.5)  // this might not alternate server on a new set
			setResult <- mkSet(0, 0, p1Sets + p2Sets + 1, server)  // calls mkSet() and gets the result when the set is over
			nextSet <- if (setResult) mkMatch(p1Sets + 1, p2Sets)
			           else mkMatch(p1Sets, p2Sets + 1)
		} yield nextSet
	}

	def mkSet(p1Games: Int, p2Games: Int, setNum: Int, server: Boolean): Element[Boolean] = {
		// if (server) println("--- P1 serving (" + p1Games + ":" + p2Games + ") ---")
		// else println("--- P2 serving (" + p1Games + ":" + p2Games + ") ---")

		if ((setNum != 3) && (p1Games == 6) && (p2Games == 6)) tiebreak(0, 0, server)
		else if ((p1Games >= 6) && ((p1Games - p2Games) >= 2)) Constant(true)
		else if ((p2Games >= 6) && ((p2Games - p1Games) >= 2)) Constant(false)
		else for {
				  gameResult <- Flip( if (server) probP1Serve else probP2Serve )
				  nextGame <- if (gameResult) mkSet(p1Games + 1, p2Games, setNum, !server)
				              else mkSet(p1Games, p2Games + 1, setNum, !server)
			} yield nextGame			
	}

	/* Objective: A player needs to win 7 points first and be up by 2 points
	 * Rule: The player, who would have served on the game after 6-6, is the first server.
	 *		 The first server only serves the first point, and then for the rest of the tiebreak,
	 *       the players alternate serves for 2 points
	 */
	def tiebreak(p1Points: Int, p2Points: Int, server: Boolean): Element[Boolean] = {
		// if (((p1Points + p2Points) % 2) == 1) server = !server

		if ((p1Points >= 7) && ((p1Points - p2Points) >= 2)) Constant(true)
		else if ((p2Points >= 7) && ((p2Points - p1Points) >= 2)) Constant(false)
		else for {
			server <- (((p1Points + p2Points) % 2) == 1) !server
			pointResult <- Flip( if (server) probP1Serve else probP2Serve )
			nextPoint <- if (pointResult) tiebreak(p1Points + 1, p2Points, server)
					     else tiebreak(p1Points, p2Points + 1, server)
		} yield nextPoint
	}

	def main(args: Array[String]) {
		println("Win Probability of P1: " + Importance.probability(Match, true))
	}
}