// title: HelloWorld.scala
// name: Jun Soo Shin
// date: 4/27/2016 - 5/10/2016
// note: The entire code is from the book "Practical Probabilistic Programming" by
//       Avi Pfeffer on pages 24-25. I'm just trying to see how this code works.

// import Figaro constructs
import com.cra.figaro.language.{Flip, Select}
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination

// define the model
object HelloWorld {
	val sunnyToday = Flip(0.2)
	val greetingToday = If(sunnyToday,
		Select(0.6 -> "Hello, world!", 0.4 -> "Howdy, universe!"),
		Select(0.2 -> "Hello, world!", 0.8 -> "Oh no, not again!"))
	val sunnyTomorrow = If(sunnyToday, Flip(0.8), Flip(0.05))
	val greetingTomorrow = If(sunnyTomorrow,
		Select(0.6 -> "Hello, world!", 0.4 -> "Howdy, universe!"),
		Select(0.2 -> "Hello, world!", 0.8 -> "Oh no, not again"))

	// predict today's greeting using an inference algorithm
	def predict() {
		val result = VariableElimination.probability(greetingToday, "Hello, world!")
		println("Today's greeting is \"Hello, world!\" with probability " 
				+ result + ".")	
	}

	// use an inference algorithm to infer today's weather, given the observation that
	// today's greeting is "Hello, world!"
	def infer() {
		greetingToday.observe("Hello, world!")
		val result = VariableElimination.probability(sunnyToday, true)
		println("If today's greeting is \"Hello, world!\", today's "
			    + "weather is sunny with probability " + result + ".")
	}

	// learn from observing that today's greeting is "Hello, world!" to predict
	// tomorrow's greeting using an inference algorithm
	def learnAndPredict() {
		greetingToday.observe("Hello, world!")
		val result = VariableElimination.probability(greetingTomorrow, "Hello, world!")
		println("If today's greeting is \"Hello, world!\", "
				+ "tomorrow's greeting will be \"Hello, world!\" "
				+ "with probability " + result + ".")
	}

	// main method that performs all the tasks
	def main(args: Array[String]) {
		predict()
		infer()
		learnAndPredict()
	}
}