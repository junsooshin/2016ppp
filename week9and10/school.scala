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

	val students = Array.fill(10)(new Student)
	val subjects = Array.fill(3)(new Subject)
	val instructors = Array.fill(2)(new Instructor)

	val grade0 = new Grade(students(0), subjects(0), instructors(0))
	val grade1 = new Grade(students(0), subjects(1), instructors(1))
	val grade2 = new Grade(students(1), subjects(0), instructors(0))

	def main(args: Array[String]) {
		println("hi")
		val algorithm = Importance(500, students(0).ability, subjects(0).difficulty, 
			                       instructors(0).quality)
		algorithm.start()
		println("Student 0's ability: " + algorithm.mean(students(0).ability))
		println("Subject 0's difficulty: " + algorithm.mean(subjects(0).difficulty))
		println("Instructor 0's quality: " + algorithm.mean(instructors(0).quality))
		algorithm.kill()
	}
}