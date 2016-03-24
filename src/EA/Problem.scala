/******************************************************************************
  *
  * Especification of combinatorial problems to be solved
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

trait Problem[Gene] {
  // number of variables in chromosome
  def numVars : Int

  // evaluates the chromosome corresponding to a solution
  def computeFitness(ind : Individual[Gene]) : Fitness

  // checks if solution is optimal
  def isOptimal(ind : Individual[Gene]) : Boolean
}