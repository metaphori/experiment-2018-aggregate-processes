package it.unibo.replicated

import java.util.Comparator

import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.interfaces.Node
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.{CustomSpawn, ScafiAlchemistSupport}


object Metrics {
}

class Gossip extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with FieldUtils with CustomSpawn
  with BlockT with BlockG with BlockC with BlockS {
  override type MainResult = Any

  import Builtins.Bounded

  def gossipNaive[T:Bounded](value: T) = {
    maxHood(value)
  }

  def gossipGC[T](value: T)(implicit ev: Bounded[T]) = {
    val leader = S(grain = Double.PositiveInfinity, metric = nbrRange)
    env.put("leader", leader)
    broadcast(leader, C[Double,T](
      potential = distanceTo(leader),
      acc = ev.max(_,_),
      local = value,
      Null = ev.bottom))
  }

  def gossipReplicated[T](value: T, p: Double, k: Int)(implicit ev: Bounded[T]) = {
    val clock = sharedTimerWithDecay[Double](p, dt(whenNan = 0)).toLong
    val isNewClock = captureChange(clock)
    val newProcs = if(isNewClock) Set(clock) else Set[Long]()
    val replicates = sspawn[Long,Unit,T]((pid: Long) => (_) => {
      import Status._
      (gossipGC[T](value), if(clock - pid < k){ Output } else { External })
    }, newProcs, ())

    env.put("clock", clock)
    env.put("dt", dt(whenNan = 0))
    env.put("replicates", replicates)

    (replicates ++ Map[Long,T](Long.MaxValue -> value)).minBy[Long](_._1)._2
  }

  val f: java.util.function.Function[_>:Node[Any],_<:Double] = _.getConcentration(new SimpleMolecule("sensor")).asInstanceOf[Double]
  def gossipOracle(): Double = environment.getNodes.stream().map[Double](f)
    .max((o1: Double, o2: Double) => o1.compareTo(o2)).get()

  def senseValue = scala.util.Random.nextDouble()*200
  var sensedValue: Double = _

  override def main = {
    sensedValue = senseValue
    env.put[Double]("sensor", sensedValue)

    env.put("cycltimr", cyclicTimerWithDecay(10,dt(whenNan = 0)))
    val gnaive = gossipNaive(sensedValue)
    val ggc = gossipGC(sensedValue)
    val grep = gossipReplicated(sensedValue, p = 5, k = 3)
    val gopt = gossipOracle()
    env.put("gossip_naive", gnaive)
    env.put("gossip_GC", ggc)
    env.put("gossip_repl", grep)
    env.put("gossip_opt", gopt)
    grep
  }

  /************ FUNCTIONS *************/

  def impulsesEvery[T : Numeric](d: T, dt: T): Boolean =
    rep(false){ impulse =>
      branch(impulse) { false } { T(d,dt)==0 }
    }

  def captureChange[T](x: T) = rep((x,false)) { case (value, _) =>
    (x, value != x)
  }._2

  override def sharedTimerWithDecay[T](period: T, dt: T)(implicit ev: Numeric[T]): T =
    rep(ev.zero) { clock =>
      val clockPerceived = foldhood(clock)(ev.max)(nbr(clock))
      branch (ev.compare(clockPerceived, clock) <= 0) {
        // I'm currently as fast as the fastest device in the neighborhood, so keep on counting time
        ev.plus(clock, (branch(cyclicTimerWithDecay(period, dt)) { ev.one }  { ev.zero }))
      } {
        // Someone else's faster, take his time, and restart counting
        clockPerceived
      }
    }

  override def cyclicTimerWithDecay[T](length: T, decay: T)(implicit ev: Numeric[T]): Boolean = {
    val x = rep(length){ left =>
      branch (left == ev.zero) {
        length
      } {
        T(length, decay)
      }
    }
    env.put("x", x)
  x == length}
}