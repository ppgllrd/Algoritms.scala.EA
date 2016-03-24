/******************************************************************************
  *
  * Evolutionary Algorithms
  *
  * @ Pepe Gallardo, 2016
  *
  *****************************************************************************/

package EA

import java.util.Random

import EA.util.Logger

case class EAParams
  ( popSize : Int = 100
  , crossProb : Probability = 0.9
  , mutProb : Probability
  , maxRunTime : Seconds = Double.MaxValue
  , maxIters : Int = Int.MaxValue
  ) {
  override def toString =
    "EAParams(popSize=%d, crossProb=%.3f, mutProb=1/%.2f, maxRunTime=%.2f segs.)".format(popSize,crossProb,1.0/mutProb,maxRunTime)
}


// Current state of EA
case class EAState[Gene]
  ( population : Population[Gene]
  , best : Individual[Gene]
  , var iter : Int
  , logger : Logger
  , rnd : Random
  , params : EAParams
  , problem : Problem[Gene]
  )


// Information returned after running EA
case class EAResult[Gene]
  ( best : Individual[Gene] // best solution found
  , population: Population[Gene]
  , lastIter : Int // last iteration done
  , bestIter : Int // iteration where best sol was found
  , logger: Logger
  ) {
  override def toString =
    "EAResult(best=%s, lastIter=%d, bestIter=%d)".format(best, lastIter, bestIter)
}


abstract class EA[Gene](seed : Int, val params : EAParams, val problem : Problem[Gene]) {
  protected val population : Population[Gene]

  // should also assign fitness to new individual
  def initialize(ind : Individual[Gene], idx : Int, eaState : EAState[Gene])

  def evaluate(ind : Individual[Gene], eaState : EAState[Gene]) : Fitness = {
    problem.computeFitness(ind)
  }

  def mutate(ind : Individual[Gene], eaState : EAState[Gene])

  def select(eaState : EAState[Gene]) : Individual[Gene]

  def recombine(child : Individual[Gene], parent1 : Individual[Gene], parent2 : Individual[Gene], eaState : EAState[Gene])

  def replace(ind : Individual[Gene], eaState : EAState[Gene])

  def endCondition(eaState: EAState[Gene]) : Boolean

  def run() : EAResult[Gene]
}


abstract class SteadyStateEA[Gene : Manifest](seed: Int, logger: Logger, params: EAParams, problem: Problem[Gene])
          extends EA[Gene](seed, params, problem) {

  override def run() : EAResult[Gene] = {
    val state =
      EAState[Gene](
          population = population
        , best = new Individual[Gene](problem.numVars)
        , iter = 0
        , logger = logger
        , rnd = new Random(seed)
        , params = params
        , problem = problem
        )

    println("Seed=%d".format(seed))
    println(params)

    // initialize population
    population.initialize(state)

    val ind = new Individual[Gene](problem.numVars)

    state.best.copyFrom(state.population.best())

    state.logger.register(state.iter, state.best.fitness)

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

      ind.fitness = evaluate(ind, state)

      if(ind.fitness > state.best.fitness) {
        state.best.copyFrom(ind)
        state.logger.register(state.iter, state.best.fitness)
        bestIter = state.iter
      }

      replace(ind, state)
    }
    EAResult( best = state.best
            , population = state.population
            , lastIter = state.iter
            , bestIter = bestIter
            , logger = state.logger
            )
  }
}


trait StandardOperators[Gene] {
  def select(eaState : EAState[Gene]) =
    Selection.binaryTournament(eaState.population, eaState.rnd)

  def recombine(child : Individual[Gene], parent1 : Individual[Gene], parent2 : Individual[Gene], eaState : EAState[Gene]) =
    Recombination.uniform(child, parent1, parent2, eaState.rnd)

  def replace(ind : Individual[Gene], eaState : EAState[Gene]) {
    Replacement.worst(eaState.population, ind)
  }
}

abstract class StandardOperatorsSteadyStateEA[Gene : Manifest](seed: Int, logger: Logger, params: EAParams, problem: Problem[Gene])
  extends SteadyStateEA[Gene](seed, logger, params, problem)
  with StandardOperators[Gene] {
  override val population = StandardPopulation[Gene](params.popSize, this)
}


abstract class StandardOperatorsSteadyStateNonRepeatedPopEA[Gene : Manifest](seed: Int, logger: Logger, params: EAParams, problem: Problem[Gene])
  extends SteadyStateEA[Gene](seed, logger, params, problem)
  with StandardOperators[Gene] {
  override val population = NonRepeatedPopulation(params.popSize, this)
}


class StandardParams[Gene](problem : Problem[Gene])
  extends EAParams( popSize = 100
                   , crossProb = 0.9
                   , mutProb = 1.0/problem.numVars
                   )


trait TimedEA[Gene] {
  val problem : Problem[Gene]

  def endCondition(eaState: EAState[Gene]) =
    eaState.logger.timer.elapsedTime() > eaState.params.maxRunTime || problem.isOptimal(eaState.best)
}


abstract class StandardSteadyStateTimedEA[Gene : Manifest](seed: Int, logger: Logger, override val problem: Problem[Gene], maxRunTime: Seconds)
  extends StandardOperatorsSteadyStateEA[Gene](seed, logger, new StandardParams(problem).copy(maxRunTime = maxRunTime), problem)
  with TimedEA[Gene]


abstract class StandardSteadyStateNonRepeatedPopTimedEA[Gene : Manifest](seed: Int, logger: Logger, override val problem: Problem[Gene], maxRunTime: Seconds)
  extends StandardOperatorsSteadyStateNonRepeatedPopEA[Gene](seed, logger, new StandardParams(problem).copy(maxRunTime = maxRunTime), problem)
  with TimedEA[Gene]


trait IteratedEA[Gene] {
  val problem : Problem[Gene]

  def endCondition(eaState: EAState[Gene]) =
    eaState.iter >= eaState.params.maxIters || problem.isOptimal(eaState.best)
}


abstract class StandardSteadyStateIteratedEA[Gene : Manifest](seed: Int, logger: Logger, override val problem: Problem[Gene], maxIters: Int)
  extends StandardOperatorsSteadyStateEA[Gene](seed, logger, new StandardParams(problem).copy(maxIters = maxIters), problem)
  with IteratedEA[Gene]


abstract class StandardSteadyStateNonRepeatedPopIteratedEA[Gene : Manifest](seed: Int, logger: Logger, override val problem: Problem[Gene], maxIters: Int)
  extends StandardOperatorsSteadyStateNonRepeatedPopEA[Gene](seed, logger, new StandardParams(problem).copy(maxIters = maxIters), problem)
  with IteratedEA[Gene]


trait BinaryRandomInitialization {
  def initialize(ind : Individual[Bit], idx : Int, eaState : EAState[Bit]) {
    BinaryInitialization.random(ind, eaState.rnd)
    ind.fitness = eaState.problem.computeFitness(ind)
  }
}


trait BinaryBitFlipMutation {
  def mutate(ind: Individual[Bit], eaState: EAState[Bit]) {
    BinaryMutation.flipBit(ind, eaState.params.mutProb, eaState.rnd)
  }
}


case class StandardSteadyStateIteratedBinaryEA(seed: Int, logger: Logger, override val problem: Problem[Bit], maxIters: Int)
  extends StandardSteadyStateIteratedEA[Bit](seed, logger, problem, maxIters)
  with BinaryRandomInitialization
  with BinaryBitFlipMutation


case class StandardSteadyStateNonRepeatedPopIteratedBinaryEA(seed: Int, logger: Logger, override val problem: Problem[Bit], maxIters: Int)
  extends StandardSteadyStateNonRepeatedPopIteratedEA[Bit](seed, logger, problem, maxIters)
  with BinaryRandomInitialization
  with BinaryBitFlipMutation


case class StandardSteadyStateTimedBinaryEA(seed: Int, logger: Logger, override val problem: Problem[Bit], maxRunTime: Seconds)
  extends StandardSteadyStateTimedEA[Bit](seed, logger, problem, maxRunTime)
  with BinaryRandomInitialization
  with BinaryBitFlipMutation


case class StandardSteadyStateNonRepeatedPopTimedBinaryEA(seed: Int, logger: Logger, override val problem: Problem[Bit], maxRunTime: Seconds)
  extends StandardSteadyStateNonRepeatedPopTimedEA[Bit](seed, logger, problem, maxRunTime)
  with BinaryRandomInitialization
  with BinaryBitFlipMutation