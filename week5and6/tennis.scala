import com.cra.figaro.language._
import com.cra.figaro.library.compound.If

object tennisExercise {

	def tennisMatch(p1ProbServeWin: Double, p2ProbServeWin: Double): Element[Boolean] = {

		def set(p1Serve: Boolean, p1ProbServeWin: Double, p2ProbServeWin: Double,
			      p1GamesWon: Int, p2GamesWon: Int): Element[Int] = {
			p1GamesWon = Apply(p1GamesWon, (p1: Int) =>
				if (p1Serve)
					if (Flip(p1ProbServeWin) == true) p1 + 1 else p1)
			// 	} else {
			// 		p2GamesWon = Apply(p2GamesWon, (p2: Int) =>	p2 = p2 + 1)
			// 	}
			// } else {
			// 	if (Flip(p2ProbServeWin)) p2GamesWon = p2GamesWon + 1; else p1GamesWon = p1GamesWon + 1
			
			val setCompleted = Apply(p1GamesWon, p2GamesWon, (p1Games: Int, p2Games: Int) =>
				((p1GamesWon == 6) || (p2GamesWon == 6)))
			// If(setCompleted, returns the winner, calls set() again)
		}

		val p1Serve = Flip(0.5)
		

	}

	def main(args: Array[String]) {
		// println(tennisMatch(0.6, 0.5))
	}
}