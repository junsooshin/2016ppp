/* title: hiddenmarkov.scala
 * name: Jun Soo Shin
 * date: 5 July 2016
 * note: exercise in chapter 10 (Factored inference algorithms) of the Practical
 *       Probabilistic Programming book
 */

import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored.VariableElimination

object hiddenMarkov {
	val length = 1000

	val confident:  Array[Element[Boolean]] = Array.fill(length)(Constant(false))
	val possession: Array[Element[Boolean]] = Array.fill(length)(Constant(false))

	// connects the hidden states and the observations
	def connect() = {
		confident(0) = Flip(0.4)

		for { minute <- 1 until length } {  // ties current confident with previous confident
			confident(minute) = If(confident(minute - 1), Flip(0.6), Flip(0.3))
		}

		for { minute <- 0 until length } {  // ties current possession with current confident
			possession(minute) = If(confident(minute), Flip(0.7), Flip(0.3))
		}
	}

	// observes the observation sequence (possessions)
	def observe() {
		for { minute <- 0 until length } {
			possession(minute).observe(scala.util.Random.nextBoolean())
		}
	}

	// runs the VE algorithm and times its execution
	def timer(): Double = {
		val alg = VariableElimination(confident(length - 1))
	    val timeBefore = System.currentTimeMillis()
	    alg.start()
	    val timeAfter = System.currentTimeMillis()
	    println("probability: " + alg.probability(confident(length - 1), true))
	    (timeAfter - timeBefore) / 1000.0
	}

	def main(args: Array[String]) {
		connect()
		observe()
		println(length + ": " + timer())
	}
}