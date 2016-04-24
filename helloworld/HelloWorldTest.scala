import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
 import com.cra.figaro.algorithm.factored.VariableElimination

object HelloWorldTest{
	def main(args: Array[String]) {
		val helloWorldElement = Select(0.8->"Hello World!", 0.2->"Goodbye World!")
		val sampleHelloWorld = VariableElimination(helloWorldElement)

		sampleHelloWorld.start()

		println("Probability of Hello World:")
		println(sampleHelloWorld.probability(helloWorldElement, "Hello World!"))
		println("Probability of Goodbye World:")
		println(sampleHelloWorld.probability(helloWorldElement, "Goodbye World!"))
	}
}