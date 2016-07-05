/* title: hiddenmarkov.scala
 * name: Jun Soo Shin
 * date: 5 July 2016
 * note: exercise in chapter 10 (Factored inference algorithms) of the Practical
 *       Probabilistic Programming book
 */

import com.cra.figaro.language._
import com.cra.figaro.library.compound.{IF}
import com.cra.figaro.algorithm.factored.VariableElimination

object hiddenMarkov {

	def initiate(length: Int) {
		val confident:  Array[Element[Boolean]] = Array.fill(length)(Constant(false))
		val possession: Array[Element[Boolean]] = Array.fill(length)(Constant(false))

		confident(0) = Flip(0.4)

		for { minute <- 1 until length } {  // ties current confident with previous confident
			confident(minute) = If(confident(minute - 1), Flip(0.6), Flip(0.4))
		}

		for { minute <- 0 until length } {  // ties current possession with current confident
			possession(minute) = If(confident(minute), Flip(0.7), Flip(0.3))
		}
	}

	def observe(length: Int) {
		
	}

	def timer(length: Int): Double = {
		val alg = VariableElimination(confident(length - 1))
	    val timeBefore = System.currentTimeMillis()
	    alg.start()
	    val timeAfter = System.currentTimeMillis()
	    (timeAfter - timeBefore) / 1000.0
	}

	def main(args: Array[String]) {
		val length = 90
		initiate(length)

		println(length + ": " + timer(length))
	}
}