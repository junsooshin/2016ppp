// title: chapter2examples.scala
// name: Jun Soo Shin
// date: 4/27/2016 - 5/10/2016
// note: The entire code is from the chapter 2 of the book "Practical 
//       Probabilistic Programming" by Avi Pfeffer. I'm just following 
//       along the examples on the book.

import com.cra.figaro.language._
import com.cra.figaro.library.compound.If

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance

import com.cra.figaro.library.atomic.discrete.Binomial
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.library.atomic.continuous.Uniform

object chapter2examples {

	def binomialTest() {
		val numSunnyDaysInWeek = Binomial(7, 0.2)
		println(VariableElimination.probability(numSunnyDaysInWeek, 7))
	}

	def normalTest() {
		val temperature = Normal(40, 100)
		def greaterThan50(d: Double) = d > 50
		println(Importance.probability(temperature, greaterThan50 _))
	}

	def uniformTest() {
		val temperature = Uniform(10, 70)
		def greaterThan50(d: Double) = d > 50
		println(Importance.probability(temperature, greaterThan50 _))
	}

	def compoundTest() {
		val tempMean = Normal(40, 9)
		val temperature = Normal(tempMean, 100)
		println(Importance.probability(temperature, (d:Double) => d > 40))
	}

	def applyTest() {
		val sunnyDaysInMonth = Binomial(30, 0.2)
		def getQuality(i: Int): String =
			if (i > 10) "good"; else if (i > 5) "average"; else "poor"
		val monthQuality = Apply(sunnyDaysInMonth, getQuality)
		println(VariableElimination.probability(monthQuality, "good"))
	}

	def chainTest() {
		val sunnyDaysInMonth = Binomial(30, 0.2)
		val teamWinsInMonth = Binomial(5, 0.4)
		val monthQuality = Apply(sunnyDaysInMonth, teamWinsInMonth,
			(days: Int, wins: Int) => {
				val x = days * wins
				if (x > 20) "good"; else if (x > 10) "average"; else "poor"
			})
		
		val goodMood = Chain(monthQuality, (s: String) =>
			if (s == "good") Flip(0.9)
			else if (s == "average") Flip(0.6)
			else Flip(0.1))

		println(VariableElimination.probability(goodMood, true))
	}

	def constraintTest(){
		val result1 = Flip(0.4)
		val result2 = Flip(0.4)
		val result3 = Flip(0.4)
		val allWins = Apply(result1, result2, result3,
			(w1: Boolean, w2: Boolean, w3: Boolean) => w1 && w2 && w3)

		def makeStreaky(r1: Element[Boolean], r2: Element[Boolean]) {
			val pair = Apply(r1, r2, (b1: Boolean, b2: Boolean) => (b1, b2))
			pair.setConstraint((bb: (Boolean, Boolean)) =>
				if (bb._1 == bb._2) 1.0; else 0.5
			)
		}

		makeStreaky(result1, result2)
		makeStreaky(result2, result3)
		println(VariableElimination.probability(allWins, true))
	}

	def exercise3() {
		// val x = Flip(0.4)
		// val y = Flip(0.4)
		// val z = x
		// val w = x === z
		// println(VariableElimination.probability(w, true))

		val x = Flip(0.4)
		val y = Flip(0.4)
		val z = y
		val w = x === z
		println(VariableElimination.probability(w, true))
	}

	def main(args: Array[String]) {
		constraintTest()
	}
}