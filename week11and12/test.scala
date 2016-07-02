import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.atomic.continuous._

object test {
	def main(args: Array[String]) {
		// Universe.createNew()
		val x = Beta(1, 1)
		val y = Flip(x)
		println(VariableElimination.probability(y, true))
	}
}