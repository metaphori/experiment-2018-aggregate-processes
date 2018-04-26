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
    val lastPid = sharedTimerWithDecay(period, dt(whenNan = 0)).toLong
    val newProcs = if(captureChange(lastPid)) Set(lastPid) else Set[Long]()
    sspawn[Long,T,R]((pid: Long) => (arg) => {
      (proc(arg), if(lastPid - pid < numReplicates){ Output } else { External })
    }, newProcs, argument)
  }

  trait GenerationInSpace {
    def where(pred: Boolean): GenerationInSpaceContinuation

    def inNode(id: ID): GenerationInSpaceContinuation = where(mid==id)
    def inNodes(ids: ID*): GenerationInSpaceContinuation = where(ids.contains(mid))
  }

  trait GenerationInTime {
    def when(pred: Boolean): GenerationInTimeContinuation

    def after(delay: Double, dt: Double): GenerationInTimeContinuation = when(T(delay, dt)==0)
    def every(period: Double, dt: Double): GenerationInTimeContinuation = when(cyclicTimerWithDecay(period, dt))

    def once: GenerationInTimeContinuation = after(0, 0)
  }

  trait KeyGenerator {
    def generateKeys[K](k: Long => List[K]): KeyGeneratorContinuation[K]
  }

  class GenerationInSpaceContinuation(val inSpace: Boolean) extends GenerationInTime {
    override def when(pred: Boolean) = new GenerationInTimeContinuation(inSpace & pred)
  }

  class GenerationInTimeContinuation(val inSpaceTime: Boolean) extends KeyGenerator {
    override def generateKeys[K](kgen: Long => List[K]): KeyGeneratorContinuation[K] =
      new KeyGeneratorContinuation[K](inSpaceTime, kgen(rep(0L){ k => if(inSpaceTime) k+1 else k }))
  }

  trait ProcGenerator[K] {
    def run[A,R](proc: A => R): ProcContinuation[K,A,R]
  }

  class KeyGeneratorContinuation[K](inSpaceTime: Boolean, keys: List[K]) extends ProcGenerator[K] {
    override def run[A, R](proc: (A) => R): ProcContinuation[K, A, R] =
      new ProcContinuation[K,A,R](keys.toSet, proc)
  }

  object spawn extends GenerationInSpace {
    override def where(pred: Boolean) = new GenerationInSpaceContinuation(pred)
  }

  class ProcContinuation[K,A,R](keys: Set[K], proc: A => R) {
    def withArgs(args: A) = spawn[K,A,R](k => a => (proc(a),true), keys, args)
  }

  override def main = {
    //val a = generate.when(mid == 10 & rep(0)(_+1)==20).generate[Unit,Long](_ => rep(0L)(_+1) ).withArgs(_)
    //env.put("a", a)
    spawn
      .where(mid == 2)
      .every(30, dt())
      .generateKeys[Long](k => List(k))
      .run[Int,Int]{
        rep(_)(_+1)
      }.withArgs(1000)
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