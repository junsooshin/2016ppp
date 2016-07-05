/* title: firm.scala
 * name: Jun Soo Shin
 * date: 5 July 2016
 * note: exercise in chapter 8 (Modeling dynamic systems) of the Practical 
 *       Probabilistic Programming book
 */

import com.cra.figaro.language._
import com.cra.figaro.library.compound.{^^}
import com.cra.figaro.algorithm.filtering.ParticleFilter

object firmExercise {
	val investmentFraction = 0.1
	val initial = Universe.createNew()
	Constant(100000.0)("capital", initial)
	Constant(0.0)("investment", initial)

	// given the previous capital and previous investment,
	// returns the new investment, new profit, and new capital in a tuple
	def transition(capital: Double, investment: Double): 
				   (Element[(Double, Double, Double)]) = {

		val newProfit = Select(0.7 -> investment * 1.15,
							   0.1 -> investment,
							   0.2 -> investment * 0.95)

		val newInvestment: Element[Double] = Constant(capital * investmentFraction)

		val newCapital = Apply(newProfit, newInvestment,
							   (p: Double, i: Double)
							   => capital + p - i)

		^^(newCapital, newInvestment, newProfit)
	}

	def nextUniverse(previous: Universe): Universe = {
		val next = Universe.createNew()
		val prevCapital = previous.get[Double]("capital")
		val prevInvestment = previous.get[Double]("investment")
		val newState = Chain(prevCapital, prevInvestment, transition _)
		Apply(newState, (s: (Double, Double, Double)) => s._1)("capital", next)
		Apply(newState, (s: (Double, Double, Double)) => s._2)("investment", next)
		Apply(newState, (s: (Double, Double, Double)) => s._3)("profit", next)
		next
	}

	def main(args: Array[String]) {
		val alg = ParticleFilter(initial, nextUniverse, 10000)
		alg.start()
		for { time <- 1 to 20 } {
			val evidence = List()  // no evidence
			alg.advanceTime(evidence)
			print("Time " + time + ": ")
			println("expected capital = " + alg.currentExpectation("capital", (c: Double) => c))
		}
		alg.stop()
	}
}