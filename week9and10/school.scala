import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Uniform
import com.cra.figaro.algorithm.sampling.Importance

object schoolExercise {

	case class Student(ability:Element[Double])
	case class Subject(difficulty:Element[Double])
	case class Instructor(quality:Element[Double])

	// grade ranges from 0.0 to 4.0; average grade is 2.0
	class Choice(student: Student, subject: Subject, instructor: Instructor) {
		val estimate = Apply(student.ability, subject.difficulty, instructor.quality,
						     (ability: Double, difficulty: Double, quality: Double)
						      => ability * 2 + quality - difficulty)
		val grade = Apply(estimate, (est: Double)
			              => est + 1)
	}

	val students = Array.fill(2)(new Student(Uniform(0, 1)))
	val subjects = Array.fill(2)(new Subject(Uniform(0, 1)))
	val instructors = Array.fill(2)(new Instructor(Uniform(0 ,1)))

	// for inference
	val choice0 = new Choice(students(0), subjects(0), instructors(0))

	// for prediction
	val possibleChoice0 = new Choice(students(0), subjects(1), instructors(0))
	val possibleChoice1 = new Choice(students(0), subjects(1), instructors(1))
	val nextChoice = Select(0.5 -> possibleChoice0, 0.5 -> possibleChoice1)
	val nextGrade = Chain(nextChoice, (c: Choice)
	                      => c.grade)

	def infer() {
		choice0.grade.addCondition(_ > 3.49)  // if student 0 got grade of ~3.5 on
		choice0.grade.addCondition(_ < 3.51)  // on subject 0 taught by instructor 0 ...

		val algorithm = Importance(1500, students(0).ability, subjects(0).difficulty, 
								   instructors(0).quality, choice0.grade)
		algorithm.start()
		println("INFERENCE >>>")
		println("Student 0's ability: " + algorithm.mean(students(0).ability))
		println("Subject 0's difficulty: " + algorithm.mean(subjects(0).difficulty))
		println("Instructor 0's quality: " + algorithm.mean(instructors(0).quality))
		println("Student 0's grade : " + algorithm.mean(choice0.grade))
		algorithm.kill()
	}

	def predict() {
		instructors(0).quality.addCondition(_ < 0.3)
		instructors(1).quality.addCondition(_ < 0.3)

		val algorithm = Importance(1500, students(0).ability, subjects(1).difficulty, 
								   instructors(0).quality, instructors(1).quality, 
								   nextGrade)
		algorithm.start()
		println("PREDICTION >>>")
		println("Student 0's ability: " + algorithm.mean(students(0).ability))
		println("Subject 1's difficulty: " + algorithm.mean(subjects(1).difficulty))
		println("Instructor 0's quality: " + algorithm.mean(instructors(0).quality))
		println("Instructor 1's quality: " + algorithm.mean(instructors(1).quality))
		println("Student 0's expected grade on nextChoice: " + algorithm.mean(nextGrade))
		algorithm.kill()
	}

	def main(args: Array[String]) {
		// infer()
		predict()
	}
}