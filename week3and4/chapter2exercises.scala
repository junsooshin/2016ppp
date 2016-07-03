/* title: chapter2test.scala
 * name: Jun Soo Shin
 * date: 4/27/2016 - 5/10/2016
 * note: The exercises 4, 5, and 6 from the chapter 2 (A quick Figaro tutorial)
 *		 of the book "Practical Probabilistic Programming" by Avi Pfeffer.
 */

import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.atomic.discrete._

object exercise4 {

	val roll1 = FromRange(1, 7)
	val roll2 = FromRange(1, 7)
	val total = Apply(roll1, roll2, (r1: Int, r2: Int) => r1 + r2)

	// the following line doesn't work. not sure why yet
	// val total = roll1 + roll2

	def main(args: Array[String]) {
		println(VariableElimination.probability(total,11))
	}
}

// how I understood the question at first:
// probability of rolling '6' on the first die and having a total greater
// than 8 after rolling the second die
//
object exercise5 {

	val roll1 = FromRange(1, 7)
	val roll2 = FromRange(1, 7)
	val req = Apply(roll1, roll2, (r1: Int, r2: Int) => 
		((r1 == 6) && ((r1 + r2) > 8)))

	def main(args: Array[String]) {
		println(VariableElimination.probability(req, true))
	}
}

// how I understood the question later:
// given that the total roll is 8, probability of rolling '6' on the first die
//
object exercise5_1 {
	val roll1 = FromRange(1, 7)
	val roll2 = FromRange(1, 7)
	val total = Apply(roll1, roll2, (r1: Int, r2: Int) => r1 + r2)

	total.setCondition((t: Int) => t > 8)  // same as observing an int greater than 8?

	def main(args: Array[String]) {
		println(VariableElimination.probability(roll1, 6))
	}
}

object exercise6 {
	val roll1 = FromRange(1, 7)
	val roll2 = FromRange(1, 7)
	val roll3 = FromRange(1, 7)
	val roll4 = FromRange(1, 7)
	val roll5 = FromRange(1, 7)
	val roll6 = FromRange(1, 7)

	val is_double1 = Apply(roll1, roll2, (r1: Int, r2: Int) => 
		if (r1 == r2) true; else false)
	val is_double2 = Apply(roll3, roll4, (r3: Int, r4: Int) => 
		if (r3 == r4) true; else false)
	val is_double3 = Apply(roll5, roll6, (r5: Int, r6: Int) => 
		if (r5 == r6) true; else false)

	val jail = is_double1 && is_double2 && is_double3

	def main(args: Array[String]) {
		println(VariableElimination.probability(jail, true))
	}
}

// this is supposed to work, too, but I couldn't get it to work
//
object exercise6_1 {
	def is_double = {
		val roll1 = FromRange(1, 7)
		val roll2 = FromRange(1, 7)
		roll1 === roll2
	}
	val jail = is_double && is_double && is_double

	def main(args: Array[String]) {
		println(VariableElimination.probability(jail, true))
	}
}

