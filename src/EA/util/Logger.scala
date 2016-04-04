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

case class Logger[Fitness]( timer : Timer = new Timer()
                           , echo : Boolean = true
                           ) {

  import EA.Fitness.fitness2Format

  class Slot( val fitness : Fitness
             , val iter : Int
             , val time : Seconds
             ) {
    override def toString = ("%10d\t"+fitness2Format(fitness)+"\t%10.2f").format(iter, fitness, time)
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


    println(("LAST: "+fitness2Format(last.fitness)).format(last.fitness))

    for(mt <- 0 to maxTime1.toInt by step) {
      val xs = slots.takeWhile(_.time <= mt)
      if(xs.nonEmpty) {
        val last = xs.last
        println(("LAST %-6d: "+fitness2Format(last.fitness)).format(mt, last.fitness))
      }
    }

    val logFormat = "%d "+fitness2Format(last.fitness)+" %.2f  "
    System.out.print("LOG: ")
    for(s <- slots)
      System.out.print(logFormat.format(s.iter, s.fitness, s.time))
    System.out.println()
  }
}
