/******************************************************************************
  *
  * Timer for measuring elapsed execution times
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA.util

case class Timer() {
  import EA.Seconds

  private var startTime = System.currentTimeMillis()

  // resets start of timer to current time
  def reset(): Unit = {
    startTime = System.currentTimeMillis()
  }

  // returns elapsed time (in seconds) since last reset
  def elapsedTime() : Seconds = {
    val t = System.currentTimeMillis()
    val segs = (t - startTime).toDouble / 1000.0
    segs
  }
}