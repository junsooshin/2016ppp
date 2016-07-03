/* title: firm.scala
 * name: Jun Soo Shin
 * date: 5 July 2016
 * note: exercise in chapter 8 of Practical Probabilistic Programming book
 */

import com.cra.figaro.language._
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.algorithm.filtering.ParticleFilter

object firmExercise {

	val initial = Universe.createNew()
	Constant(100000)("capital", initial)
	Constant(0)("investment", initial)

	// given the previous capital and previous investment,
	// returns the new investment, new profit, and new capital in a tuple
	def transition(capital: Element[Double], investment: Double): 
				   (Element[(Double, Double, Double)]) = {

		val newProfit = Select(0.7 -> investment * 1.1,
							   0.1 -> investment,
							   0.2 -> investment * 0.9)

		val newInvestment = Apply(capital, (c: Double)
								  => c / 10.0)

		val newCapital = Apply(capital, newProfit, newInvestment,
							   (c: Double, p: Double, i: Double)
							   => c + p - i)

		^^(newCapital, newInvestment, newProfit)
	}

	def nextUniverse(previous: Universe): Universe = {
		val next = Universe.createNew()
		val prevCapital = previous.get[Element[Double]]("capital")
		val prevInvestment = previous.get[Double]("investment")
		val newState = Chain(prevCapital, prevInvestment, transition _)
	    Apply(newState, (s: (Double, Double, Double)) => s._1)("capital", next)
	    Apply(newState, (s: (Double, Double, Double)) => s._2)("investment", next)
	    Apply(newState, (s: (Double, Double, Double)) => s._3)("profit", next)
		next
	}

	def main(args: Array[String]) {
		// val alg = ParticleFilter(initial, nextUniverse, 10000)
		// alg.start()


	}
}