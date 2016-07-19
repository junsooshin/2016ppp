/*  title: printer.scala
 *  name: Jun Soo Shin
 *  date: 19 July 2016
 *  note: Exercises 1-3 in chapter 12 (Solving other inference tasks) of the 
 *        Practical Probabilistic Programming book by Avi Pfeffer
 *
 *        Querying joint probability of multiple varibles, computing the most 
 *        likely values of variables, and computing the probability of an 
 *        observed evidence are the main topics in this chapter.
 *
 *        The code for the printer model is provided by the book.
 */

import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.MPEVariableElimination
import com.cra.figaro.algorithm.sampling.ProbEvidenceSampler


object PrinterProblem {


    /**************************************************************************
    *                               Printer model                             *
    **************************************************************************/


    // variables that make up the printer state
    val printerPowerButtonOn = Flip(0.95)
    val tonerLevel = Select(0.7 -> 'high, 0.2 -> 'low, 0.1 -> 'out)
    val tonerLowIndicatorOn =
        If(printerPowerButtonOn,
            CPD(tonerLevel,
                'high -> Flip(0.2),
                'low -> Flip(0.6),
                'out -> Flip(0.99)),
            Constant(false))
    val paperFlow = Select(0.6 -> 'smooth, 0.2 -> 'uneven, 0.2 -> 'jammed)
    val paperJamIndicatorOn =
        If(printerPowerButtonOn,
            CPD(paperFlow,
                'smooth -> Flip(0.1),
                'uneven -> Flip(0.3),
                'jammed -> Flip(0.99)),
            Constant(false))

    // variables that influence how the printing comes out
    val printerState =
        Apply(printerPowerButtonOn, tonerLevel, paperFlow,
            (power: Boolean, toner: Symbol, paper: Symbol) => {
                if (power) {
                    if (toner == 'high && paper == 'smooth) 'good
                    else if (toner == 'out || paper == 'out) 'out
                    else 'poor
                } else 'out
            })
    val softwareState = Select(0.8 -> 'correct, 0.15 -> 'glitchy, 0.05 -> 'crashed)
    val networkState = Select(0.7 -> 'up, 0.2 -> 'intermittent, 0.1 -> 'down)
    val userCommandCorrect = Flip(0.65)

    // variables that judge how the printing is
    val numPrintedPages =
        RichCPD(userCommandCorrect, networkState, softwareState, printerState,
            (*, *, *, OneOf('out)) -> Constant('zero),      // if printer is out
            (*, *, OneOf('crashed), *) -> Constant('zero),  // if software crashed
            (*, OneOf('down), *, *) -> Constant('zero),     // if network is down
            (OneOf(false), *, *, *) -> Select(0.3 -> 'zero, 0.6 -> 'some, 0.1 -> 'all),
            (OneOf(true), *, *, *) -> Select(0.01 -> 'zero, 0.01 -> 'some, 0.98 -> 'all))
    val printsQuickly =
        Chain(networkState, softwareState,
            (network: Symbol, software: Symbol) =>
                if (network == 'down || software == 'crashed) Constant(false)
                else if (network == 'intermittent || software == 'glitchy) Flip(0.5)
                else Flip(0.9))
    val goodPrintQuality =
        CPD(printerState,
            'good -> Flip(0.95),
            'poor -> Flip(0.3),
            'out -> Constant(false))

    val printResultSummary =
        Apply(numPrintedPages, printsQuickly, goodPrintQuality,
            (pages: Symbol, quickly: Boolean, quality: Boolean) =>
                if (pages == 'zero) 'none
                else if (pages == 'some || !quickly || !quality) 'poor
                else 'excellent)


    /**************************************************************************
    *                       Answers to the Questions                          *
    **************************************************************************/


    /*  Part 1

        This function calculates joint distribution of printer state and network 
        state, given a poor print result.
      
        Answer:
            When printer is out and/or network is down, the number of printed 
            pages is zero, and print result is none.
            Thus, the combinations consisting of those values of the variables 
            have 0 probability.

            The printer state is about 2 times more likely to be poor than good,
            and the network state is about 3 times more likely to be up than
            intermittent. The probabilities rise as the printer state gets worse
            and the network state gets better. Therefore, these two variables are 
            anti-correlated.
    */ 
    def part1() {
        val pair = ^^(printerState, networkState)
        printResultSummary.observe('poor)
        val ve = VariableElimination(pair)
        ve.start()

        println()
        println("### PART 1 ###")
        println("Given that a print result was poor,")
        println()
        println("Probability printer state is good and network state is up: " 
                + ve.probability(pair, 
                    (pair: (Symbol, Symbol)) => pair._1 == 'good && pair._2 == 'up))
        println("Probability printer state is good and network state is intermittent: " 
                + ve.probability(pair, 
                    (pair: (Symbol, Symbol)) => pair._1 == 'good && pair._2 == 'intermittent))
        println("Probability printer state is good and network state is down: " 
                + ve.probability(pair, 
                    (pair: (Symbol, Symbol)) => pair._1 == 'good && pair._2 == 'down))
        println()
        println("Probability printer state is poor and network state is up: " 
                + ve.probability(pair, 
                    (pair: (Symbol, Symbol)) => pair._1 == 'poor && pair._2 == 'up))
        println("Probability printer state is poor and network state is intermittent: " 
                + ve.probability(pair, 
                    (pair: (Symbol, Symbol)) => pair._1 == 'poor && pair._2 == 'intermittent))
        println("Probability printer state is poor and network state is down: " 
                + ve.probability(pair, 
                    (pair: (Symbol, Symbol)) => pair._1 == 'poor && pair._2 == 'down))
        println()
        println("Probability printer state is out and network state is up: " 
                + ve.probability(pair, 
                    (pair: (Symbol, Symbol)) => pair._1 == 'out && pair._2 == 'up))
        println("Probability printer state is out and network state is intermittent: " 
                + ve.probability(pair, 
                    (pair: (Symbol, Symbol)) => pair._1 == 'out && pair._2 == 'intermittent))
        println("Probability printer state is out and network state is down: " 
                + ve.probability(pair, 
                    (pair: (Symbol, Symbol)) => pair._1 == 'out && pair._2 == 'down))
        println()

        println("Probability printer state is good: " + ve.probability(pair,
            (pair: (Symbol, Symbol)) => pair._1 == 'good))
        println("Probability printer state is poor: " + ve.probability(pair,
            (pair: (Symbol, Symbol)) => pair._1 == 'poor))
        println("Probability network state is up: " + ve.probability(pair,
            (pair: (Symbol, Symbol)) => pair._2 == 'up))
        println("Probability network state is intermittent: " + ve.probability(pair,
            (pair: (Symbol, Symbol)) => pair._2 == 'intermittent))
        println()

        ve.kill()
        printResultSummary.unobserve()
    }

    /*  Part 2

        This function follows the diagnostic steps, which include finding the
        most likely values of the variables, in order to fix the poor print 
        result.

        Answer:
            After observing that a printing result was poor, the most likely
            problem was user command. If we observed that the user command was
            correct, the next most likely problem was paper flow. And if we
            observed that the paper flow was smooth, the next most likely
            problem was toner level.

            Since we have already observed that the print result was poor, 
            we will never fix the problem here. But if the print result is 
            poor, we should try to fix the user command and then the paper 
            flow, and then the toner level, and so on.
    */
    def part2() {
        printResultSummary.observe('poor)
        val MPEve = MPEVariableElimination()
        MPEve.start()
        println()
        println("### PART 2 ###")
        println("Given that a print result was poor,")
        println()
        println("Printer power button on: " + MPEve.mostLikelyValue(printerPowerButtonOn)) 
        println("Toner level: " + MPEve.mostLikelyValue(tonerLevel)) 
        println("Paper flow: " + MPEve.mostLikelyValue(paperFlow)) 
        println("Software state: " + MPEve.mostLikelyValue(softwareState)) 
        println("Network state: " + MPEve.mostLikelyValue(networkState)) 
        println("User command correct: " + MPEve.mostLikelyValue(userCommandCorrect))
        println()
        MPEve.kill()

        userCommandCorrect.observe(true)

        MPEve.start()
        println()
        println("Given that a print result was poor, and user command was correct,")
        println()
        println("Printer power button on: " + MPEve.mostLikelyValue(printerPowerButtonOn)) 
        println("Toner level: " + MPEve.mostLikelyValue(tonerLevel)) 
        println("Paper flow: " + MPEve.mostLikelyValue(paperFlow)) 
        println("Software state: " + MPEve.mostLikelyValue(softwareState)) 
        println("Network state: " + MPEve.mostLikelyValue(networkState)) 
        println("User command correct: " + MPEve.mostLikelyValue(userCommandCorrect))
        println()
        MPEve.kill()

        paperFlow.observe('smooth)

        MPEve.start()
        println()
        println("Given that a print result was poor, user command was correct, " 
                + "and paper flow is smooth,")
        println()
        println("Printer power button on: " + MPEve.mostLikelyValue(printerPowerButtonOn)) 
        println("Toner level: " + MPEve.mostLikelyValue(tonerLevel)) 
        println("Paper flow: " + MPEve.mostLikelyValue(paperFlow)) 
        println("Software state: " + MPEve.mostLikelyValue(softwareState)) 
        println("Network state: " + MPEve.mostLikelyValue(networkState)) 
        println("User command correct: " + MPEve.mostLikelyValue(userCommandCorrect))
        println()
        MPEve.kill()
    }

    /*  Part 3

        This function computes the probability of the evidence that the print
        result is poor.

        Answer: ~ 0.423
    */
    def part3() {
        val printResult =
            Apply(numPrintedPages, printsQuickly, goodPrintQuality,
                (pages: Symbol, quickly: Boolean, quality: Boolean) =>
                    if (pages == 'zero) 'none
                    else if (pages == 'some || !quickly || !quality) 'poor
                    else 'excellent)("printResult", Universe.universe)
        val evidence = List(NamedEvidence("printResult", Observation('poor)))
        println()
        println("### PART 3 ###")
        println("Probability of the evidence that a print result is poor: " +
                ProbEvidenceSampler.computeProbEvidence(100000, evidence))
        println()
    }

    def main(args: Array[String]) {
        part1()
        part2()
        part3()
    }
}
