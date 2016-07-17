/* title: printer.scala
 * name: Jun Soo Shin
 * date: 19 July 2016
 * note: Exercises 1-3 in chapter 12 (Solving other inference tasks) of the 
 *       Practical Probabilistic Programming book by Avi Pfeffer
 *
 *       Querying joint probability of multiple varibles, computing the most 
 *       likely values of variables, and computing the probability of an 
 *       observed evidence are the main topics in this chapter.
 */

import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored.VariableElimination

object PrinterProblem {

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

    def main(args: Array[String]) {
        
    }
}
