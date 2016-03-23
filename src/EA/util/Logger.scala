/******************************************************************************
  *
  * Class to log evolution of solutions along execution time
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA.util

import EA._

import scala.collection.mutable.ArrayBuffer

case class Logger( timer : Timer = new Timer()
                  , echo : Boolean = true
                  , format : String = "%10d\t%10.2f\t%10.2f"
                  , logFormat : String = "%d %.4f %.2f  ") {

  class Slot( val fitness : Fitness
             , val iter : Int
             , val time : Seconds
             ) {
    override def toString = format.format(iter, fitness, time)
  }

  class SlotWithFormat( fitness : Fitness
                       , iter : Int
                       , time : Seconds
                       , format : String
                       ) extends Slot(fitness, iter, time) {
    override def toString = format.format(iter, fitness, time)
  }

  private val slots = new ArrayBuffer[Slot]()

  private def addSlot(slot : Slot) {
    slots += slot
    if(echo)
      println(slot)
  }

  def register(iter: Int, fitness: Fitness): Unit = {
    val slot = new Slot(fitness, iter, timer.elapsedTime())
    addSlot(slot)
  }

  def register(format: String, iter: Int, fitness: Fitness): Unit = {
    val slot = new SlotWithFormat(fitness, iter, timer.elapsedTime(), format)
    addSlot(slot)
  }

  def print(maxTime : Seconds = -1, step : Int = 50): Unit = {
    if(slots.isEmpty) {
      println("No solutions!")
      return
    }

    val maxTime1 = if(maxTime<0) slots.last.time + step else maxTime
    val last = slots.last
    println(("LAST: %.4f").format(last.fitness))

    for(mt <- 0 to maxTime1.toInt by step) {
      val xs = slots.takeWhile(_.time <= mt)
      if(xs.nonEmpty) {
        val last = xs.last
        println(("LAST %-6d: %.4f").format(mt, last.fitness))
      }
    }
    System.out.print("LOG: ")
    for(s <- slots)
      System.out.print(logFormat.format(s.iter, s.fitness, s.time))
    System.out.println
  }
}
