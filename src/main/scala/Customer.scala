package customers

trait Customer {

  val processWaitTime: Double
  val alpha: Double
  val beta: Double
  val x = scala.util.Random.nextFloat

  def processTime(x: Double, a: Double, b: Double): Double = {
    val p = 200.0
    val firstChunk: Double = p * scala.math.pow(x,a-1)
    val secondChunk: Double = scala.math.pow((1 - x), (b-1))
    firstChunk * secondChunk

    //val beta = new org.apache.commons.math3.distribution.BetaDistribution(a, b)
    //beta.getAlpha
    //beta.getBeta
    //p * beta.sample
  }

}

class YellowCustomer extends Customer {
  val alpha = 2.0
  val beta = 5.0
  override val processWaitTime: Double = processTime(x=x, a = alpha, b=beta)
}

class RedCustomer extends Customer {
  val alpha = 2.0
  val beta = 2.0
  override val processWaitTime: Double = processTime(x=x, a = alpha, b=beta)
}

class BlueCustomer extends Customer {
  val alpha = 5.0
  val beta = 1.0
  override val processWaitTime: Double = processTime(x=x, a = alpha, b=beta)

}