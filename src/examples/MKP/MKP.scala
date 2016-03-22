/******************************************************************************
  *
  * The MUlktidimensional Knapsack Problem
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package examples.MKP

import EA._

class MKProblem( val values : Array[Double]
                , val constraints : Array[Array[Double]]
                , val weights : Array[Double]
                ) extends Problem {

  val numObjects : Int = values.length

  val numConstraints : Int = constraints.length

  // Check problem predonditions
  require(weights.length == numConstraints
    ,"MKProblem: number of constraints must be the same as number of weights"
  )
  require(values.forall(_ > 0)
    ,"MKProblem: All values must be > 0"
  )
  require(weights.forall(_ >= 0)
    ,"MKProblem: All weights must be >= 0"
  )
  require(constraints.forall(_.length == numObjects)
    ,"MKProblem: Each constraint must have same number of variables as number of objects"
  )
  require(constraints.forall(_.forall(_ >= 0))
    ,"MKProblem: Each constraint coefficient must be >= 0"
  )

  val heuristicOrder : Array[Int] = {
    // Para la relajación lineal, cada variable x está restringida a: 0 <= x <= 1
    //    0 <= x ya forma parte del problem, pero es necesario imponer la restricción x <= 1

    val extraConstraints: Array[Array[Double]] = new Array(numObjects)
    for (i <- 0 until numObjects) {
      val row = Array.fill[Double](numObjects)(0)
      row(i) = 1
      extraConstraints(i) = row
    }

    val extraWeights = Array.fill[Double](numObjects)(1)

    val linearRelaxation = LinearRelaxation(values, constraints ++ extraConstraints, weights ++ extraWeights)

    linearRelaxation.solve()

    println(linearRelaxation.dualSol().take(numObjects).mkString)
    new Array[Int](numObjects)
  }

  // Reads a problem from a text file
  def this(fileName : String) {
    this(null, null, null) // toDo
  }

  // Evals objective function for an assignments of vars
  def evaluate(xs : Chromosome) : Fitness = {
    require(xs.length == numObjects, "MKProblem.evaluate: Number of variables must be the same as number of objects")
    var v = 0.0
    for(i <- xs.indices)
      v += values(i) * xs(i)
    v
  }

  // Checks if an assignment staisfies all constraints
  def cumpleRestricciones(xs : Chromosome) : Boolean = {
    false // toDo
  }

  def repair(xs : Chromosome): Unit = {
    // toDo
  }

  val numberVars = numObjects

  def evalSolution(xs : Chromosome) : Fitness = {
    repair(xs)
    evaluate(xs)
  }

  def isOptimal(xs : Chromosome) : Boolean = false

  override def toString : String = {
    "toDo..." // toDo
  }
}

object MKProblem {
  def apply(values : Array[Double], constraints : Array[Array[Double]],  weights : Array[Double]) =
    new MKProblem(values, constraints, weights)
  def apply(fileName : String) =
    new MKProblem(fileName)
}


// max cx : Ax <= b, x >= 0 }.
// Assumes that b >= 0 so that x = 0 is a basic feasible solution.
case class LinearRelaxation(val c : Array[Double], val A : Array[Array[Double]], val b : Array[Double]) {
  val numVars = c.length
  val numConstraints = A.length

  require(A.forall(_.length == numVars), "LinearRelaxation: number of columns in A must be equal to number of variables")
  require(A.length == b.length, "LinearRelaxation: number of constraints must be equal to b length")

  private var opt : Option[Simplex] = None

  def solve(): Unit = {
    opt = Some(new Simplex(A,b,c))
  }

  def primalSol() : Array[Double] = opt match {
    case None => sys.error("LinearRelaxation: unsolved problem")
    case Some(simplex) => simplex.primal()
  }

  def dualSol() : Array[Double] = opt match {
    case None => sys.error("LinearRelaxation: unsolved problem")
    case Some(simplex) => simplex.dual()
  }

  def optimalValue() : Double = opt match {
    case None => sys.error("LinearRelaxation: unsolved problem")
    case Some(simplex) => simplex.value()
  }
}


