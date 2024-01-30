import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class BAC(weight: Double, time: Double, vol: Double, amount: Double, gender: String) {
  val std: Double = 0.0068

  def standardDrink(): Double = {
    BigDecimal((std * vol) * amount).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  def promilleMan(): Double = {
    BigDecimal((standardDrink() * 12) / ((weight * 1.7) - (0.15 * time))).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  def promilleWoman(): Double = {
    BigDecimal((standardDrink() * 12) / ((weight * 1.6) - (0.15 * time))).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  def result(): Unit = {
    val futureResult = Future {
      if (gender == "woman") {
        println(s"\nAs a woman who have had $amount cl. of $vol% vol, $time hour ago.")
        println(s"You've had ${standardDrink()} drinks, which gives you a ${promilleWoman() * 1000}% BAC\n")
      } else if (gender == "man") {
        println(s"\nAs a man who have had $amount cl. of $vol% vol, $time hour ago.")
        println(s"You've had ${standardDrink()} drinks, which gives you a ${promilleMan() * 1000}% BAC\n")
      } else {
        println("Fault.")
      }
    }

    // Wait for the future to complete (blocking operation)
    Await.result(futureResult, Duration.Inf)
  }

  def TOTAL(): Unit = {
    // Implement your TOTAL method here
  }
}

object BAC {
  def apply(weight: Double, time: Double, vol: Double, amount: Double, gender: String): BAC = {
    new BAC(weight, time, vol, amount, gender)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val weight = scala.io.StdIn.readLine("Weight of the patient in kg: ").toDouble
    val time = scala.io.StdIn.readLine("Enter the time of drink (in hour only): ").toDouble
    val vol = scala.io.StdIn.readLine("Volume percent of alcohol in the drink:").toDouble
    val amount = scala.io.StdIn.readLine("Enter the amount that you drank: ").toDouble
    val gender = scala.io.StdIn.readLine("You are man or woman: ")

    val bacInstance = BAC(weight, time, vol, amount, gender)
    bacInstance.result()
    // bacInstance.TOTAL()
  }
}
