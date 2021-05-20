import customers.{BlueCustomer, RedCustomer, YellowCustomer}
import scala.annotation.tailrec

object main {

  def main(args: Array[String]) = {

    def arrivalTime(t: Double): Double = {
      // probability of customer arrival given t
      //val alpha: Double = 100.0
      1 - scala.math.exp(-t / 100.0)
    }

    def newCustomer(t: Double): Boolean = {
      // simulates if customer arrives given t
      if (scala.util.Random.nextFloat < arrivalTime(t=t)) true else false
    }

    def newCustomerSim(): Double =
    // increments t by 1 until a customer arrives. Returns t at point of customer arrival
    {
      @tailrec
      def newCustomerSimRec(t: Double): Double =
      {
        if (newCustomer(t=t)) {
          return t  // time of customer arrival
        } else {
          newCustomerSimRec(t + 1.0)
        }
      }
      newCustomerSimRec(0.0)
    }

    def runSimulation(iterations: Double, func: () => Double): Statistics =
    {
      @tailrec
      def runSimulationRec(iterations: Double, customerWaitTimeList: List[Double]): List[Double] =
      {
        if (iterations == 0.0) {
          return customerWaitTimeList  // time of customer arrival
        } else {
          val newcustomerWaitTimeList = customerWaitTimeList :+ func()
          runSimulationRec(iterations - 1.0, newcustomerWaitTimeList)
        }
      }
      val answerList = runSimulationRec(iterations, List())
      val average = answerList.sum / answerList.length
      val maximum = answerList.max

      Statistics(average.round, maximum.round)
    }

    // Question 1.

    val simCustWait = runSimulation(10.0, newCustomerSim)
    println(simCustWait.average) // 12.54 after 50,000 simulations
    println(simCustWait.maximum) // 48.0 after 50,000 simulations

    // Question 2.
    val RC = new RedCustomer

    def redCustomerSim(): Double = {
      val RC = new RedCustomer
      RC.processWaitTime
    }

    val simRedCustWait = runSimulation(1000.0, redCustomerSim)
    println(simRedCustWait.average) // 33.86 after 1000 simulations
    println(simRedCustWait.maximum) // 49.99 after 1000 simulations

    // Question 3.
    def yellowCustomerSim(): Double = {
      val YC = new YellowCustomer
      YC.processWaitTime
    }

    val simYellowCustWait = runSimulation(1000.0, yellowCustomerSim)
    println(simYellowCustWait.average) // 7.225 after 1000 simulations
    println(simYellowCustWait.maximum) // 16.38 after 1000 simulations

    def blueCustomerSim(): Double = {
      val BC = new BlueCustomer
      BC.processWaitTime
    }

    val simBlueCustWait = runSimulation(1000.0, blueCustomerSim)
    println(simBlueCustWait.average) // 42.80 after 1000 simulations
    println(simBlueCustWait.maximum) // 199.23 after 1000 simulations

  }
}