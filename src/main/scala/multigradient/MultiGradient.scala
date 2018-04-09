package multigradient

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class MultiGradient extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with FieldUtils with CustomSpawn with BlockT with BlockG with BlockC {
  override type MainResult = Any

  val (idSource1, idSource2) = (0, 378)
  val generators = Set(189,285,294,125,114)
  val timeTurnOffSource2 = 100
  val startEvery = 20 // A generation impulse every 20 time units
  val numActiveProcs = 5
  val considerAfter = 40 // Consider this process output only after 40 time units

  def source = mid()==idSource1 | (mid()==idSource2 & timer(timeTurnOffSource2)>0)

  def isGenerator = generators.contains(mid)

  def delta: Int = dt(whenNan = 0).toInt

  def replicatedGossip2(src: Boolean, numActiveProcs: Int, startEvery: Int, considerAfter: Int): Double = {
    val sharedTimer = sharedTimerWithDecay(startEvery, delta)
    val impulse = captureChange(sharedTimer)

    val procs = spawn[Int,Boolean,Double](
      process = (_) => source => {
        val shutdownLimit = T(startEvery * numActiveProcs, delta)
        val outlimit = T(considerAfter, delta)
        val status = mux[Status](shutdownLimit!=0){
          mux[Status](outlimit!=0){ Bubble }{ Output }
        }{ External }
        (distanceTo(source), status)
      },
      params = mux(isGenerator & impulse){ Set(sharedTimer.toInt) }{ Set() },
      args = src)

    env.put("T_dt", delta)
    env.put("T_SharedTimer", sharedTimer)
    env.put("T_Impulse", impulse)
    env.put("Procs", procs)
    procs.lastOption.map(_._2).getOrElse(Double.PositiveInfinity)
  }
  
  /*****************************
  ** MAIN PROGRAM ENTRY POINT **
  ******************************/

  override def main() = {
    val src = source
    env.put("source", src)
    env.put("generator", isGenerator)
    env.put("gradient", distanceTo(src))
    replicatedGossip2(src, numActiveProcs, startEvery, considerAfter)
  }

  /*****************************
   ** Functions **
   ******************************/

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

  override def cyclicTimerWithDecay[T](length: T, decay: T)(implicit ev: Numeric[T]): Boolean =
    rep(length){ left =>
      branch (left == ev.zero) {
        length
      } {
        T(length, decay)
      }
    } == length
}