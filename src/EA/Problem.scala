/******************************************************************************
  *
  * Especification of combinatorial problems to be solved
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

trait Problem {
  // number of variables in chromosome
  def numberVars : Int

  // evaluates a chromosome corresponding to a solution
  def evalSolution(xs : Chromosome) : Fitness

  // checks if solution is optimal
  def isOptimal(xs : Chromosome) : Boolean
}