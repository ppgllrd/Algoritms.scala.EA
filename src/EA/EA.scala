/******************************************************************************
  *
  * Evolutionary Algorithms
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

import java.util.Random

case class EAParams( popSize : Int = 100
                    , crossProb : Probability = 0.9
                    , mutProb : Probability
                    , maxRunTime : Seconds = Double.MaxValue
                    , maxIters : Int = Int.MaxValue
                    ) {
  override def toString =
    "EAParams(popSize=%d, crossProb=%.3f, mutProb=1/%.2f, maxRunTime=%.2f segs.)".format(popSize,crossProb,1.0/mutProb,maxRunTime)
}


// Current state of EA
case class EAState( population : Population
                   , best : Individual
                   , var iter : Int
                   , timer : util.Timer
                   , rnd : Random
                   , params : EAParams
                   , problem : Problem
                   )


// Information returned after running EA
case class EAResult( best : Individual // best sol found
                    , lastIter : Int // last iteration done
                    , bestIter : Int // iteration where best sol was found
                    ) {
  override def toString =
    "EAResult(best=%s, lastIter=%d, bestIter=%d)".format(best, lastIter, bestIter)
}


abstract class EA(seed : Int, val params : EAParams, val problem : Problem) {
  protected val population : Population

  // should also assign new individual fitness
  def initialize(ind : Individual, idx : Int, eaState : EAState)

  def mutate(ind : Individual, eaState : EAState)

  def select(eaState : EAState) : Individual

  def recombine(child : Individual, parent1 : Individual, parent2 : Individual, eaState : EAState)

  def replace(ind : Individual, eaState : EAState)

  def endCondition(eaState: EAState) : Boolean

  def printIncumbent(eaState: EAState)

  def run() : EAResult
}


abstract class SteadyStateEA(seed : Int, params : EAParams, problem : Problem)
          extends EA(seed, params, problem) {
  override def run() : EAResult = {

    val state =
      EAState( population = population
             , best = Individual(problem.numVars)
             , iter = 0
             , timer = util.Timer()
             , rnd = new Random(seed)
             , params = params
             , problem = problem
             )

    state.timer.reset()

    println("Seed=%d".format(seed))
    println(params)

    // initialize population
    population.initialize(state)

    val ind = Individual(problem.numVars)

    state.best.copyFrom(state.population.best())

    printIncumbent(state)
    var bestIter = state.iter

    while(!endCondition(state)) {
      state.iter += 1

      if(state.rnd.nextDouble() < params.crossProb) {
        val parent1 = select(state)
        val parent2 = select(state)

        recombine(ind, parent1, parent2, state)
      } else
        ind.copyFrom(Selection.random(state.population, state.rnd))

      mutate(ind, state)

      ind.fitness = problem.computeFitness(ind)

      if(ind.fitness > state.best.fitness) {
        state.best.copyFrom(ind)
        printIncumbent(state)
        bestIter = state.iter
      }

      replace(ind, state)
    }
    EAResult( best = state.best
            , lastIter = state.iter
            , bestIter = bestIter
            )
  }
}


class StandardParams(problem : Problem)
  extends EAParams( popSize = 100
                   , crossProb = 0.9
                   , mutProb = 1.0/problem.numVars
                   )


abstract class StandardOperatorsSteadyStateEA(seed : Int, params : EAParams, problem : Problem)
  extends SteadyStateEA(seed, params, problem) {

  override val population : Population = StandardPopulation(params.popSize, this)

  override def initialize(ind : Individual, idx : Int, eaState : EAState) {
    Initialization.random(ind, eaState.rnd)
    ind.fitness = problem.computeFitness(ind)
  }

  override def mutate(ind : Individual, eaState : EAState) {
    Mutation.flipBit(ind, eaState.params.mutProb, eaState.rnd)
  }

  override def select(eaState : EAState) =
    Selection.binaryTournament(eaState.population, eaState.rnd)

  override def recombine(child : Individual, parent1 : Individual, parent2 : Individual, eaState : EAState) =
    Recombination.uniform(child, parent1, parent2, eaState.rnd)

  override def replace(ind : Individual, eaState : EAState) {
    Replacement.worst(eaState.population, ind)
  }

  private val format = "%10.2f\t%10d\t%10.2f"
  override def printIncumbent(eaState : EAState): Unit = {
    println(format.format(eaState.best.fitness, eaState.iter, eaState.timer.elapsedTime()))

  }
}


abstract class StandardOperatorsSteadyStateNonRepeatedPopEA(seed : Int, params : EAParams, problem : Problem)
          extends StandardOperatorsSteadyStateEA(seed, params, problem) {
  override val population = NonRepeatedPopulation(params.popSize, this)
}


trait TimedEA {
  val problem : Problem

  def endCondition(eaState: EAState) =
    eaState.timer.elapsedTime() > eaState.params.maxRunTime || problem.isOptimal(eaState.best)
}


case class StandardSteadyStateTimedEA(seed : Int, override val problem : Problem, maxRunTime : Double)
  extends StandardOperatorsSteadyStateEA( seed
                                         , new StandardParams(problem).copy(maxRunTime = maxRunTime)
                                         , problem
                                         )
  with TimedEA


case class StandardSteadyStateNonRepeatedPopTimedEA(seed : Int, override val problem : Problem, maxRunTime : Double)
  extends StandardOperatorsSteadyStateNonRepeatedPopEA( seed
                                                       , new StandardParams(problem).copy(maxRunTime = maxRunTime)
                                                       , problem
                                                       )
  with TimedEA


trait IteratedEA {
  val problem : Problem

  def endCondition(eaState: EAState) =
    eaState.iter >= eaState.params.maxIters || problem.isOptimal(eaState.best)
}


case class StandardSteadyStateIteratedEA(seed : Int, override val problem : Problem, maxIters : Int)
  extends StandardOperatorsSteadyStateEA( seed
                                         , new StandardParams(problem).copy(maxIters = maxIters)
                                         , problem
                                         )
  with IteratedEA


case class StandardSteadyStateNonRepeatedPopIteratedEA(seed : Int, override val problem : Problem, maxIters : Int)
  extends StandardOperatorsSteadyStateNonRepeatedPopEA( seed
                                                       , new StandardParams(problem).copy(maxIters = maxIters)
                                                       , problem
                                                       )
  with IteratedEA
