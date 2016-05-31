import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.algorithm.factored.VariableElimination

object tennisExercise {

	val probP1Serve = 0.6
	val probP2Serve = 0.5

	val Match: Element[Boolean] = mkMatch(0, 0)

	val Set: Element[Boolean] = for {
									b <- Flip(0.5)  // chooses the first server
									w <- mkSet(0, 0, b)
								} yield w

	def mkMatch(p1Sets: Int, p2Sets: Int): Element[Boolean] = {
		if (p1Sets == 2) Constant(true)
		else if (p2Sets == 2) Constant(false)
		else for {
			b <- Set
			w <- if (b) mkMatch(p1Sets + 1, p2Sets)
			     else mkMatch(p1Sets, p2Sets - 1)
		} yield w
	}

	def mkSet(p1Games: Int, p2Games: Int, whoServes: Boolean): Element[Boolean] = {
		if (p1Games == 6) Constant(true)
		else if (p2Games == 6) Constant(false)
		else for {
				  g <- Flip(if (whoServes) probP1Serve else probP2Serve)
				  s <- if (g) mkSet(p1Games + 1, p2Games, !whoServes)
				       else mkSet(p1Games, p2Games + 1, !whoServes)
			} yield s			
	}


	def main(args: Array[String]) {
		println(VariableElimination.probability(Match, true))
	}
}