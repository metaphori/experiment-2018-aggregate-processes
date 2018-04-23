package it.unibo.plib

import java.util.Comparator

import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.interfaces.Node
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.{CustomSpawn, ScafiAlchemistSupport}


class PLib extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with FieldUtils with CustomSpawn
  with BlockT with BlockG with BlockC with BlockS {

  import it.unibo.Spawn._

  override type MainResult = Any

  def replicated[T,R](proc: T => R)(argument: T, period: Double, numReplicates: Int) = {
    val lastPid = sharedTimerWithDecay[Double](period, dt(whenNan = 0)).toLong
    val newProcs = if(captureChange(lastPid)) Set(lastPid) else Set[Long]()
    sspawn[Long,T,R]((pid: Long) => (arg) => {
      (proc(arg), if(lastPid - pid < numReplicates){ Output } else { External })
    }, newProcs, argument)
  }

  class PGenContinuation[A,R](proc: A => R){
    def on(pred: => Boolean) = spawn[Long,A,R](_ => args => (proc(args), true),
      if(pred) Set(timestamp()) else Set(), ???)
    def every(period: Long) = on(timestamp() % period == 0)
  }
  def generate[A,R](proc: A => R) = new PGenContinuation(proc)

  implicit class ProcGenerator[K](gen: => List[K]) {
    def generate[A,R](proc: A => R) = new ProcContinuation(() => gen, proc)
  }
  class ProcContinuation[K,A,R](gen: () => List[K], proc: A => R) {
    def withArgs(args: A) = spawn[K,A,R](k => a => (proc(a),true), gen().toSet, args)
  }

  object generate {
    def when(pred: Boolean) = new ProcGenerator[Long](
      if(pred) List(timestamp()) else List()
    )
    def once = new ProcGenerator[Long](
      if(rep(0L)(_+1)==1) List(timestamp()) else List()
    )
    def every(period: Double, dt: Double) = new ProcGenerator[Long]({
      val clock = clockWithDecay[Double](period, dt).toLong
      if(captureChange(clock)) List(clock) else List[Long]()
    })
  }

  trait GenerationInSpace {
    def inNode(id: ID)
    def inNodes(ids: ID*)
  }

  override def main = {
    val a = generate.when(mid == 10 & rep(0)(_+1)==20).generate[Unit,Long](_ => rep(0L)(_+1) ).withArgs(_)
    env.put("a", a)
  }

  /************ FUNCTIONS *************/

  def captureChange[T](x: T, initially: Boolean = true) = rep((Option.empty[T],false)) { case (value, _) =>
    (Some(x), value.map(_ != x).getOrElse(initially))
  }._2

  def clockWithDecay[T](period: T, dt: T)(implicit ev: Numeric[T]): T =
    rep(ev.zero) { clock =>
      ev.plus(clock, (branch(cyclicTimerWithDecay(period, dt)) { ev.one }  { ev.zero }))
    }
}