import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Uniform
import com.cra.figaro.algorithm.sampling.Importance

object schoolExercise {

	class Student {
		val ability = Uniform(0, 1)
	}

	class Subject {
		val difficulty = Uniform(0 ,1)
	}

	class Instructor {
		val quality = Uniform(0, 1)
	}


	class Grade(student: Student, subject: Subject, instructor: Instructor) {
		val estimate = Apply(student.ability, subject.difficulty, instructor.quality,
						     (ability: Double, difficulty: Double, quality: Double)
						     => ability * 2 + quality - difficulty)
		val grade = Apply(estimate, (est: Double)
			              => if (est > 1.0) 1.0 else est)
	}



	def main(args: Array[String]) {
		println("hi")
	}
}