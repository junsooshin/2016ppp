import com.cra.figaro.language._
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.algorithm.filtering.ParticleFilter

object firmExercise {

	val initial = Universe.createNew()
	Constant(100000)("capital", initial)
	Constant(0)("investment", initial)

	// given the previous capital and previous investment,
	// returns the new investment, new profit, and new capital
	//
	def transition(capital: Int, investment: Int): (Element[(Int, Int, Int)]) = {
		val newProfit = Select(0.7 -> investment * 1.1,
							   0.1 -> investment,
							   0.2 -> investment * 0.9)
		val newInvestment = Apply(capital, (c: Int) => c / 10)
		val newCapital = Apply(capital, newProfit, newInvestment, (c: Int, p: Int, i: Int)
							   => c + p - i)
		^^(newProfit, newInvestment, newCapital)
	}

	def nextUniverse(previous: Universe): Universe = {
		val next = Universe.createNew()


		next
	}

	def main(args: Array[String]) {
		// val alg = ParticleFilter(initial, nextUniverse, 10000)
		// alg.start()


	}
}